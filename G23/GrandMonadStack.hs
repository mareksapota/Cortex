module Cortex.G23.GrandMonadStack
    ( LesserMonadStack
    , GrandMonadStack
    ) where

import Control.Concurrent.Lifted (MVar)
import Control.Monad.State (StateT)
import Control.Monad.Error (ErrorT)
import Data.Set (Set)

type LesserMonadStack = ErrorT String IO

-- State holds Miranda host and port.
type GrandMonadStack = StateT (String , Int) LesserMonadStack
