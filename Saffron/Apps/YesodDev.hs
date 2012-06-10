module Cortex.Saffron.Apps.YesodDev
    ( prepare
    , run
    ) where

-----

import Control.Concurrent.Lifted
import Control.Monad.Trans (liftIO)
import Control.Monad.Error (throwError)
import Control.Monad (when)
import System.Process (waitForProcess)
import System.Exit (ExitCode (ExitSuccess))

import Cortex.Saffron.GrandMonadStack
import qualified Cortex.Saffron.Apps.Common as Common
import Cortex.Common.ErrorIO

-----

makePath :: String -> Maybe String
makePath location = Just $ location ++ "/repo"

-----

prepare :: String -> AppManagerMonadStack ()
prepare location = do
    let path = makePath location

    proc1 <- iRunProcess "cabal" ["install"] path Nothing
    exit1 <- liftIO $ waitForProcess proc1
    when (exit1 /= ExitSuccess) $ throwError "cabal install error"

-----

run :: Int -> String -> AppManagerMonadStack (MVar (), MVar Int)
run port location = Common.run port "yesod"
    [ "devel"
    , "--port"
    , show port
    ] (Just $ location ++ "/repo") Nothing

-----
