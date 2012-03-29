{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import Control.Monad.State
import Control.Monad.Error
import Control.Concurrent.Lifted

import Adria.Common.OptParse (CmdArgs)
import Adria.Common.MonadOptParse
import qualified Adria.Common.OptParse as OptParse
import Adria.Common.ErrorIO

type StateVars = (Int, String, MVar Int, MVar ())

makeArgs :: ErrorT String IO CmdArgs
makeArgs = execOptParseT $ do
    addOption ["-p", "--port"] "port"
        "Middleware server port (default is 9426)"
    addOption ["--host"] "host"
        "Middleware server location (default is localhost)"
    addOption ["--messages"] "messages"
        "How many messages should be sent (default is 10)"

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
    (messages :: Int) <- OptParse.getOptionWithDefault args "messages" 10
    running <- liftIO $ newMVar messages
    block <- liftIO newEmptyMVar
    runStateT (run messages) (port, host, running, block)
    -- Discard runStateT output and return correct type.
    return ()

run :: Int -> StateT StateVars (ErrorT String IO) ()
run instances = do
    sequence_ [fork runProducer | _ <- [1..instances]]
    (_, _, _, block) <- get
    when (instances > 0) (liftIO $ takeMVar block)

runProducer :: StateT StateVars (ErrorT String IO) ()
runProducer = do
    catchError runProducer' reportError
    producerStop
    where
        reportError e = iPutStrLn stderr $ "Error: " ++ e

runProducer' :: StateT StateVars (ErrorT String IO) ()
runProducer' = do
    (port, host, _, _) <- get
    hdl <- iConnectTo host port
    iSetBuffering hdl LineBuffering
    iPutStrLn hdl "publish"
    iPutStrLn hdl "test"
    iPutStrLn hdl ""
    iPutStrLn hdl "test message"
    iClose hdl

producerStop :: StateT StateVars (ErrorT String IO) ()
producerStop = do
    (_, _, running, block) <- get
    producers <- liftIO $ takeMVar running
    let updated_producers = producers - 1
    liftIO $ putMVar running updated_producers
    when (updated_producers == 0) (liftIO $ putMVar block ())
