{-# LANGUAGE FlexibleContexts #-}

module Cortex.Common.IOReport (ioReport) where

import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Error (MonadError, throwError)
import Control.Exception (try, IOException)

-----

ioReport :: (MonadError String m, MonadIO m) => IO a -> m a
ioReport f = do
    t <- liftIO $ try f
    case t of
        Left e -> throwError $ show (e :: IOException)
        Right a -> return a
