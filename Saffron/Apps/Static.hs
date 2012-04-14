module Cortex.Saffron.Apps.Static
    ( prepare
    , run
    ) where

-----

import Control.Concurrent.Lifted

import Cortex.Saffron.GrandMonadStack
import qualified Cortex.Saffron.Apps.Common as Common
import Cortex.Common.ErrorIO

-----

prepare :: String -> AppManagerMonadStack ()
prepare location = Common.prepare $
    iRawSystem "cp" ["Saffron/Apps/static.ru", location]

-----

run :: Int -> String -> AppManagerMonadStack (MVar (), MVar Int)
run port location = Common.run port $ concat
    [ "thin -R '../static.ru' -c '"
    , location
    , "/repo' -p "
    , show port
    , " start &> /dev/null"
    ]

-----
