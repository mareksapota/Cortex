module Cortex.Common.Error
    ( finally
    , ignoreError
    ) where

import Control.Monad.Error (catchError, MonadError)

finally :: MonadError a m => m () -> m b -> m b
finally s f = do
    { ignoreError s
    ; f
    }

ignoreError :: MonadError a m => m () -> m ()
ignoreError s = s `catchError` (\_ -> return ())
