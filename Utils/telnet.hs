{-# LANGUAGE ScopedTypeVariables #-}

-- Telnet like tool that doesn't use CRLF end of line symbol as normal telnet
-- does.

import Control.Concurrent.Lifted
import Control.Monad.Error (runErrorT, ErrorT, throwError, catchError)
import Control.Monad
import Control.Monad.Trans (liftIO)
import System.IO
import Data.Maybe (isNothing, fromJust)

import Cortex.Common.ErrorIO
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
    iSetBuffering hdl LineBuffering
    fork $ catchError (fromServer hdl) (reportError lock)
    fork $ catchError (toServer hdl) (\_ -> liftIO $ putMVar lock ())
    liftIO $ takeMVar lock
    where
        reportError lock e = do
            iPutStrLn stderr $ "Error: " ++ e
            iPutStrLn stderr $ "Error: Connection to server terminated"
            putMVar lock ()

fromServer :: Handle -> ErrorT String IO ()
fromServer hdl = forever $ do
    l <- iGetLine hdl
    iPutStrLn stdout l
    iFlush stdout

toServer :: Handle -> ErrorT String IO ()
toServer hdl = forever $ do
    l <- iGetLine stdin
    iPutStrLn hdl l
    iFlush hdl
