{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Error (ErrorT, runErrorT, catchError)
import Control.Monad.State
import System.IO
import Control.Concurrent.Lifted

import Cortex.Common.OptParse (CmdArgs)
import Cortex.Common.MonadOptParse
import qualified Cortex.Common.OptParse as OptParse
import Cortex.Common.Error
import Cortex.Common.ErrorIO

type StateVars = (String, Int, MVar (Int, Int), MVar ())

makeArgs :: ErrorT String IO CmdArgs
makeArgs = execOptParseT $ do
    addOption ["--host"] "host"
        "Host Miranda runs on (default is 127.0.0.1)"
    addOption ["-p", "--port"] "port"
        "Port Miranda runs on (default is 8205)"
    -- When you issue N messages it will translate to at least 3*N remote
    -- operations.
    addOption ["--messages"] "messages"
        "How many messages should be sent (default is 10)"
    addOption ["--concurrency"] "concurrency"
        "How many concurrent requests should be performed (default is 10)"

main :: IO ()
main = do
    e <- runErrorT main'
    report e
    where
        report (Left s) = hPutStrLn stderr $ "Error: " ++ s
        report (Right _) = return ()

main' :: ErrorT String IO ()
main' = do
    -- Buffer stderr output to get sane log messages, not mixed up random
    -- letters.
    iSetBuffering stderr LineBuffering
    options <- makeArgs
    args <- OptParse.evalArgs options
    (host :: String) <- OptParse.getOptionWithDefault args "host" "127.0.0.1"
    (port :: Int) <- OptParse.getOptionWithDefault args "port" 8205
    (messages :: Int) <- OptParse.getOptionWithDefault args "messages" 10
    (concurrency :: Int) <- OptParse.getOptionWithDefault args "concurrency" 10
    let atOnce = min messages concurrency
    let toDo = messages
    let next = messages
    mv <- newMVar (toDo, next)
    block <- newEmptyMVar
    runStateT (run atOnce) (host, port, mv, block)
    return ()

run :: Int -> StateT StateVars (ErrorT String IO) ()
run atOnce = do
    (_, _, _, block) <- get
    sequence_ [fork runOne | _ <- [1..atOnce]]
    when (atOnce > 0) (takeMVar block)
    where
        runOne = do
            (_, _, mv, _) <- get
            (toDo, next) <- takeMVar mv
            putMVar mv (toDo, next - 1)
            runInstance next

runInstance :: Int -> StateT StateVars (ErrorT String IO) ()
runInstance i = do
    { do
        { perform_set
        ; perform_get
        ; perform_delete
        } `catchError` retry
    ; (_, _, mv, block) <- get
    ; (toDo, next) <- takeMVar mv
    ; putMVar mv (toDo - 1, next - 1)
    ; when (next > 0) (runInstance next)
    ; when (toDo - 1 == 0) $ putMVar block ()
    }
    where
        retry e = do
            reportError e
            runInstance i

        perform_delete = do
            (host, port, _, _) <- get
            hdl <- iConnectTo host port
            iSetBuffering hdl (BlockBuffering $ Just 32)
            iPutStrLn hdl "delete"
            iPutStrLn hdl $ concat ["test::", show i]
            iClose hdl

        perform_set = do
            (host, port, _, _) <- get
            hdl <- iConnectTo host port
            iSetBuffering hdl (BlockBuffering $ Just 32)
            iPutStrLn hdl "set"
            iPutStrLn hdl $ concat ["test::", show i]
            iPutStrLn hdl $ show i
            iClose hdl

        perform_get = do
            (host, port, _, _) <- get
            hdl <- iConnectTo host port
            iSetBuffering hdl (BlockBuffering $ Just 32)
            iPutStrLn hdl "lookup"
            iPutStrLn hdl $ concat ["test::", show i]
            iFlush hdl
            response <- iGetLine hdl
            iClose hdl
            when (response /= concat ["Just ", show i]) perform_get
