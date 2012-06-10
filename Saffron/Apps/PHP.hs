module Cortex.Saffron.Apps.PHP
    ( prepare
    , run
    ) where

-----

import Control.Monad (liftM)
import Control.Concurrent.Lifted
import Text.StringTemplate
import qualified Data.ByteString.Lazy.Char8 as LBS

import Cortex.Saffron.GrandMonadStack
import qualified Cortex.Saffron.Apps.Common as Common
import Cortex.Common.ErrorIO

-----

prepare :: String -> AppManagerMonadStack ()
prepare _ = return ()

-----

run :: Int -> String -> AppManagerMonadStack (MVar (), MVar Int)
run port location = do
    conf <- liftM LBS.unpack $ iReadFile "Saffron/Apps/apache.php.conf"
    let template = newSTMP conf :: StringTemplate String
    let conf' = LBS.pack $ toString $ setAttribute "PORT" port $
            setAttribute "LOCATION" (location ++ "/repo") template
    iWriteFile (location ++ "/apache.conf." ++ (show port)) conf'
    Common.run port "apache2"
        [ "-f"
        , location ++ "/apache.conf." ++ (show port)
        , "-D"
        , "FOREGROUND"
        , "-k"
        , "start"
        ] Nothing Nothing

-----
