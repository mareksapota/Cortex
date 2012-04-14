module Cortex.Ariel.GrandMonadStack
    ( LesserMonadStack
    , GrandMonadStack
    ) where

import Control.Monad.State (StateT)
import Control.Monad.Error (ErrorT)

type LesserMonadStack = ErrorT String IO

-- State holds Miranda host, port, proxy configuration file location and
-- currently proxied instances hash.
type GrandMonadStack = StateT (String , Int, String, String) LesserMonadStack
