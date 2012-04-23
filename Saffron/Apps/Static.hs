module Cortex.Saffron.Apps.Static
    ( prepare
    , run
    ) where

-----

import Control.Concurrent.Lifted
import Text.StringTemplate

import Cortex.Saffron.GrandMonadStack
import qualified Cortex.Saffron.Apps.Common as Common
import Cortex.Common.ErrorIO

-----

prepare :: String -> AppManagerMonadStack ()
prepare _ = return ()

-----

run :: Int -> String -> AppManagerMonadStack (MVar (), MVar Int)
run port location = do
    conf <- iReadFile "Saffron/Apps/nginx.static.conf"
    let template = newSTMP conf :: StringTemplate String
    let conf' = toString $ setAttribute "PORT" port $
            setAttribute "LOCATION" (location ++ "/repo") template
    iWriteFile (location ++ "/nginx.conf." ++ (show port)) conf'
    Common.run port "nginx"
        [ "-c"
        , location ++ "/nginx.conf." ++ (show port)
        ] Nothing Nothing

-----
