{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Error (runErrorT, ErrorT, throwError)
import System.IO
import Control.Concurrent.Lifted
import Control.Monad
import Data.Maybe (isNothing, fromJust)

import Cortex.Common.OptParse (CmdArgs)
import Cortex.Common.MonadOptParse
import qualified Cortex.Common.OptParse as OptParse
import Cortex.Common.ErrorIO
import Cortex.Miranda.GrandMonadStack
import Cortex.Miranda.Storage (runStorage)
import Cortex.Miranda.Server (runServer)

makeArgs :: ErrorT String IO CmdArgs
makeArgs = execOptParseT $ do
    addOption ["-p", "--port"] "port"
        "Miranda will listen on this port (default is 8205)"
    addOption ["-s", "--storage"] "storage"
        "Miranda storage location (no storage is used by default)"

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

main' :: LesserMonadStack ()
main' = do
    -- Buffer stderr output to get sane log messages, not mixed up random
    -- letters.
    iSetBuffering stderr LineBuffering
    options <- makeArgs
    args <- OptParse.evalArgs options
    (port :: Int) <- OptParse.getOptionWithDefault args "port" 8205
    (storage :: Maybe String) <- OptParse.getOption args "storage"
    when (isNothing storage) $ throwError "You didn't specify a storage dir"
    runStorage (runServer port) (fromJust storage)
