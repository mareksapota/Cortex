module Cortex.Ariel.GrandMonadStack
    ( LesserMonadStack
    , GrandMonadStack
    ) where

import Control.Monad.State (StateT)
import Control.Monad.Error (ErrorT)
import qualified Data.ByteString.Char8 as BS

import Cortex.Common.Miranda

type LesserMonadStack = ErrorT String IO

-- State holds Miranda host, port, proxy configuration file location and
-- currently proxied instances hash.
type GrandMonadStack = StateT (MirandaInfo, String, BS.ByteString) LesserMonadStack
