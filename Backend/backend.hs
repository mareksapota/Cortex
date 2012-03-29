{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Error (runErrorT, ErrorT)
import System.IO
import Control.Concurrent.Lifted

import Adria.Common.OptParse (CmdArgs)
import Adria.Common.MonadOptParse
import qualified Adria.Common.OptParse as OptParse
import Adria.Common.ErrorIO

import Adria.Backend.Storage (runVS)
import Adria.Backend.Server (runServer)

makeArgs :: ErrorT String IO CmdArgs
makeArgs = execOptParseT $ do
    addOption ["-p", "--port"] "port"
        "Backend will listen on this port (default is 8205)"
    addOption ["--bus-port"] "bus-port"
        "Middleware server port (default is 9426)"
    addOption ["--bus-host"] "bus-host"
        "Middleware server location (default is localhost)"
    addOption ["-s", "--storage"] "storage"
        "Backend storage location (no storage is used by default)"

main :: IO ()
main = do
    lock <- newEmptyMVar
    -- Main thread is a bound thread, let's escape that.
    fork $ do
        e <- runErrorT main'
        reportError e
        putMVar lock ()
    -- Wait for the spawned thread to exit.
    takeMVar lock
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
    (port :: Int) <- OptParse.getOptionWithDefault args "port" 8205
    (busPort :: Int) <- OptParse.getOptionWithDefault args "bus-port" 9426
    (busHost :: String) <- OptParse.getOptionWithDefault
        args "bus-host" "localhost"
    (storage :: Maybe String) <- OptParse.getOption args "storage"
    runVS busHost busPort (runServer port storage)
