module Cortex.Saffron.Apps.DjangoDev
    ( prepare
    , run
    ) where

-----

import Control.Concurrent.Lifted

import Cortex.Saffron.GrandMonadStack
import qualified Cortex.Saffron.Apps.Common as Common

-----

prepare :: String -> AppManagerMonadStack ()
prepare _ = return ()

-----

run :: Int -> String -> AppManagerMonadStack (MVar (), MVar Int)
run port location = Common.run port "python"
    [ "manage.py"
    , "runserver"
    , "0.0.0.0:" ++ (show port)
    ] (Just $ location ++ "/repo") Nothing

-----
