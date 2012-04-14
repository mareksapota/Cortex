{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Error (runErrorT, ErrorT)
import System.IO
import Control.Concurrent.Lifted

import Cortex.Common.OptParse (CmdArgs)
import Cortex.Common.MonadOptParse
import qualified Cortex.Common.OptParse as OptParse
import Cortex.Common.ErrorIO
import Cortex.Ariel.GrandMonadStack
import Cortex.Ariel.Proxy

makeArgs :: ErrorT String IO CmdArgs
makeArgs = execOptParseT $ do
    addOption ["--host"] "host"
        "Host Miranda runs on (default is 127.0.0.1)"
    addOption ["-p", "--port"] "port"
        "Port Miranda runs on (default is 8205)"

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
    (host :: String) <- OptParse.getOptionWithDefault args "port" "127.0.0.1"
    (port :: Int) <- OptParse.getOptionWithDefault args "port" 8205
    runProxy host port
