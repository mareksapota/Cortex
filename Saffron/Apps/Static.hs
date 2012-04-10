module Cortex.Saffron.Apps.Static
    ( run
    ) where

-----

import Control.Concurrent.Lifted
import Control.Monad.State
import System.IO (stderr)
import System.Process (runCommand, waitForProcess, terminateProcess)

import Cortex.Saffron.GrandMonadStack
import Cortex.Common.ErrorIO
import Cortex.Common.Error

-----

run :: Int -> String -> AppManagerMonadStack (MVar (), MVar Int)
run port location = do
    stop <- newEmptyMVar
    finished <- newEmptyMVar
    ignoreError $ iRawSystem "cp" ["Saffron/Apps/static.ru", location]
    fork $ do
        { p <- liftIO $ runCommand $ "thin -R '../static.ru' -c '" ++
            location ++ "/repo' -p " ++ (show port) ++ " start"

        ; fork $ do
            { takeMVar stop
            ; liftIO $ terminateProcess p
            }

        ; liftIO $ waitForProcess p
        ; putMVar finished port
        ; (_, _, app, _) <- get
        ; iPutStrLn stderr $ app ++ " instance stopped on port " ++ (show port)
        }
    return (stop, finished)
