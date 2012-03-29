module Adria.Middleware.Types (ESI) where

import Control.Monad.State (StateT)
import Control.Monad.Error (ErrorT)

type ESI a b = ErrorT String (StateT a IO) b
