{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import Control.Monad.State
import Control.Monad.Error
import Control.Concurrent.Lifted

import Adria.Common.OptParse (CmdArgs)
import Adria.Common.MonadOptParse
import qualified Adria.Common.OptParse as OptParse
import Adria.Common.ErrorIO

type StateVars = (Int, String, Int, MVar Int, MVar ())

makeArgs :: ErrorT String IO CmdArgs
makeArgs = execOptParseT $ do
    addOption ["-p", "--port"] "port"
        "Middleware server port (default is 9426)"
    addOption ["--host"] "host"
        "Middleware server location (default is localhost)"
    addOption ["--instances"] "instances"
        "How many consumers should be spawned (default is 10)"
    addOption ["--messages"] "messages"
        "How many messages should be received (default is unlimited)"

main :: IO ()
main = do
    e <- runErrorT main'
    reportError e
    where
        reportError (Left s) = hPutStrLn stderr $ "Error: " ++ s
        reportError (Right _) = return ()

main' :: ErrorT String IO ()
main' = do
    -- Buffer stderr output to get sane log messages, not mixed up random
    -- letters.
    iSetBuffering stderr LineBuffering
    options <- makeArgs
    args <- OptParse.evalArgs options
    (port :: Int) <- OptParse.getOptionWithDefault args "port" 9426
    (host :: String) <- OptParse.getOptionWithDefault args "host" "localhost"
    (instances :: Int) <- OptParse.getOptionWithDefault args "instances" 10
    (messages :: Int) <- OptParse.getOptionWithDefault args "messages" (-1)
    running <- liftIO $ newMVar instances
    block <- liftIO newEmptyMVar
    runStateT (run instances) (port, host, messages, running, block)
    -- Discard runStateT output and return correct type.
    return ()

run :: Int -> StateT StateVars (ErrorT String IO) ()
run instances = do
    sequence_ [fork $ runConsumer | _ <- [1..instances]]
    (_, _, _, _, block) <- get
    when (instances > 0) (liftIO $ takeMVar block)

runConsumer :: StateT StateVars (ErrorT String IO) ()
runConsumer = do
    catchError runConsumer' reportError
    consumerStop
    where
        reportError e = iPutStrLn stderr $ "Error: " ++ e

runConsumer' :: StateT StateVars (ErrorT String IO) ()
runConsumer' = do
    (port, host, messages, _, _) <- get
    hdl <- iConnectTo host port
    iSetBuffering hdl LineBuffering
    iPutStrLn hdl "subscribe"
    iPutStrLn hdl "+test"
    catchError (waitForMessages messages hdl)
        (\_ -> iPutStrLn stderr "Connection terminated")
    iClose hdl

waitForMessages :: Int -> Handle -> StateT StateVars (ErrorT String IO) ()
waitForMessages messages hdl
    | messages < 0 = forever $ ibGetLine hdl
    | otherwise = sequence_ [ibGetLine hdl | _ <- [1..messages]]

consumerStop :: StateT StateVars (ErrorT String IO) ()
consumerStop = do
    (_, _, _, running, block) <- get
    consumers <- liftIO $ takeMVar running
    let updated_consumers = consumers - 1
    liftIO $ putMVar running updated_consumers
    when (updated_consumers == 0) (liftIO $ putMVar block ())
