{-# LANGUAGE ScopedTypeVariables #-}

-- Telnet like tool that doesn't use CRLF end of line symbol as normal telnet
-- does.

import Control.Concurrent.Lifted
import Control.Monad.Error (runErrorT, ErrorT, throwError, catchError)
import Control.Monad
import Control.Monad.Trans (liftIO)
import System.IO
import Data.Maybe (isNothing, fromJust)

import Cortex.Common.ErrorIO (iConnectTo, iPrintLog)
import Cortex.Common.LazyIO
import Cortex.Common.MonadOptParse
import Cortex.Common.OptParse (CmdArgs)
import qualified Cortex.Common.OptParse as OptParse

makeArgs :: ErrorT String IO CmdArgs
makeArgs = execOptParseT $ do
    addOption ["-p", "--port"] "port"
        "telnet will connect to this port"
    addOption ["--host"] "host"
        "telnet will connect to this host (default is localhost)"

main :: IO ()
main = do
    e <- runErrorT main'
    reportError e
    where
        reportError (Left s) = hPutStrLn stderr $ "Error: " ++ s
        reportError (Right _) = return ()

main' :: ErrorT String IO ()
main' = do
    lock <- liftIO newEmptyMVar
    options <- makeArgs
    args <- OptParse.evalArgs options
    (port :: Maybe Int) <- OptParse.getOption args "port"
    (host :: String) <- OptParse.getOptionWithDefault args "host" "localhost"
    when (isNothing port) (throwError "You have to specify port")
    hdl <- iConnectTo host (fromJust port)
    hIn <- lConvert stdin
    hOut <- lConvert stdout
    fork $ catchError (fromServer hOut hdl) (reportError lock)
    fork $ catchError (toServer hIn hdl) (\_ -> liftIO $ putMVar lock ())
    liftIO $ takeMVar lock
    where
        reportError lock e = do
            iPrintLog $ "Error: " ++ e
            iPrintLog $ "Error: Connection to server terminated"
            putMVar lock ()

fromServer :: LazyHandle -> LazyHandle -> ErrorT String IO ()
fromServer hOut hdl = forever $ do
    l <- lGetLine hdl
    lPutStrLn hOut l
    lFlush hOut

toServer :: LazyHandle -> LazyHandle -> ErrorT String IO ()
toServer hIn hdl = forever $ do
    l <- lGetLine hIn
    lPutStrLn hdl l
    lFlush hdl
