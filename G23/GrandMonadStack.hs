module Cortex.G23.GrandMonadStack
    ( LesserMonadStack
    , GrandMonadStack
    ) where

import Control.Monad.State (StateT)
import Control.Monad.Error (ErrorT)

type LesserMonadStack = ErrorT String IO

-- State holds Miranda host and port.
type GrandMonadStack = StateT (String , Int) LesserMonadStack
