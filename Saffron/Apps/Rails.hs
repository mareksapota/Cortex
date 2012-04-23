module Cortex.Saffron.Apps.Rails
    ( prepare
    , run
    ) where

-----

import Control.Monad.Error (throwError)
import Control.Concurrent.Lifted
import Control.Monad.Trans (liftIO)
import Control.Monad (when)
import System.Process (waitForProcess)
import System.Exit (ExitCode (ExitSuccess))
import System.Environment (getEnvironment)

import Cortex.Saffron.GrandMonadStack
import qualified Cortex.Saffron.Apps.Common as Common
import Cortex.Common.ErrorIO

-----

makeEnv :: AppManagerMonadStack (Maybe [(String, String)])
makeEnv = do
    e <- liftIO getEnvironment
    return $ Just (("RAILS_ENV", "production"):e)

makePath :: String -> Maybe String
makePath location = Just $ location ++ "/repo"

-----

prepare :: String -> AppManagerMonadStack ()
prepare location = do
    env <- makeEnv
    let path = makePath location

    proc1 <- iRunProcess "bundle" ["install"] path env
    exit1 <- liftIO $ waitForProcess proc1
    when (exit1 /= ExitSuccess) $ throwError "bundle install error"
    proc2 <- iRunProcess "bundle"
        [ "exec"
        , "rake"
        , "db:migrate"
        ] path env
    exit2 <- liftIO $ waitForProcess proc2
    when (exit2 /= ExitSuccess) $ throwError "rake db:migrate error"

-----

run :: Int -> String -> AppManagerMonadStack (MVar (), MVar Int)
run port location = do
    env <- makeEnv
    let path = makePath location
    Common.run port "passenger"
        [ "start"
        , "--port"
        , show port
        ] path env

-----
