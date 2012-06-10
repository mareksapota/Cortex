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
import qualified Data.ByteString.Lazy.Char8 as LBS

import Cortex.Common.Miranda

type LesserMonadStack = ErrorT String IO

-- State holds Miranda host, port and set of already known apps.
type ManagerMonadStack = StateT
    ( MirandaInfo
    , MVar (Set LBS.ByteString)
    ) LesserMonadStack

-- State holds Miranda host, port, app name, list of dependent threads (should
-- stop notification and is stopped notification tuple), app type, app source
-- hash and app source location on disc.
type AppManagerState =
    ( MVar [(MVar (), MVar Int)]
    , MVar LBS.ByteString
    , MVar LBS.ByteString
    , MVar (Maybe String)
    )

type AppManagerMonadStack = StateT
    ( MirandaInfo
    , LBS.ByteString
    , MVar AppManagerState
    ) LesserMonadStack
