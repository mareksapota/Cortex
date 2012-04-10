module Cortex.Saffron.GrandMonadStack
    ( LesserMonadStack
    , ManagerMonadStack
    , AppManagerMonadStack
    , AppManagerState
    ) where

import Control.Concurrent.Lifted (MVar)
import Control.Monad.State (StateT)
import Control.Monad.Error (ErrorT)
import Data.Set (Set)

type LesserMonadStack = ErrorT String IO

-- State holds Miranda host, port and set of already known apps.
type ManagerMonadStack = StateT
    ( String
    , Int
    , MVar (Set String)
    ) LesserMonadStack

-- State holds Miranda host, port, app name, list of dependent threads (should
-- stop notification and is stopped notification tuple), app type, app source
-- hash and app source location on disc.
type AppManagerState =
    ( MVar [(MVar (), MVar Int)]
    , MVar String
    , MVar String
    , MVar (Maybe String)
    )

type AppManagerMonadStack = StateT
    ( String
    , Int
    , String
    , MVar AppManagerState
    ) LesserMonadStack
