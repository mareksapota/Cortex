{-# LANGUAGE FlexibleContexts #-}

module Cortex.Common.Error
    ( finally
    , ignoreError
    , reportError
    ) where

import Control.Monad.Error (catchError, MonadError)
import Control.Monad.Trans (MonadIO)

import Cortex.Common.ErrorIO

-----

finally :: MonadError a m => m () -> m b -> m b
finally s f = do
    { ignoreError s
    ; f
    }

-----

ignoreError :: MonadError a m => m () -> m ()
ignoreError s = s `catchError` (\_ -> return ())

-----

reportError :: (MonadError String m, MonadIO m) => String -> m ()
reportError e = iPrintLog $ "Error: " ++ e

-----
