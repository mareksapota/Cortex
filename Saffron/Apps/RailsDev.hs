module Cortex.Saffron.Apps.RailsDev
    ( prepare
    , run
    ) where

-----

import Control.Concurrent.Lifted
import Control.Monad.Trans (liftIO)
import System.Environment (getEnvironment)

import Cortex.Saffron.GrandMonadStack
import qualified Cortex.Saffron.Apps.Common as Common

-----

makeEnv :: AppManagerMonadStack (Maybe [(String, String)])
makeEnv = do
    e <- liftIO getEnvironment
    return $ Just (("RAILS_ENV", "development"):e)

-----

prepare :: String -> AppManagerMonadStack ()
prepare _ = return ()

-----

run :: Int -> String -> AppManagerMonadStack (MVar (), MVar Int)
run port location = do
    env <- makeEnv
    Common.run port "rails"
        [ "server"
        , "--port"
        , show port
        ] (Just $ location ++ "/repo") env

-----
