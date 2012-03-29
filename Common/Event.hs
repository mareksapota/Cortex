{-# LANGUAGE FlexibleContexts #-}

module Adria.Common.Event
    ( timer
    , periodicTimer
    ) where

import System.Random
import Control.Monad.Base
import Control.Concurrent.Lifted
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control
import Control.Monad (forever)

-----

timer :: (MonadBaseControl IO m, MonadBase IO m) => Double -> m () -> m ()
timer delay f = do
    fork $ run delay f
    return ()

-----

periodicTimer :: (MonadBaseControl IO m, MonadBase IO m, MonadIO m) =>
    Double -> m () -> m ()
periodicTimer delay f = do
    fork $ do
        -- Start somewhere between `0.0` and `delay` seconds.
        t <- liftIO $ randomRIO (0.0, delay)
        threadDelay $ round $ (10 ** 6) * t
        -- First call.
        f
        -- Start the event loop.
        forever $ run delay f
    return ()

-----

run :: (MonadBase IO m) => Double -> m () -> m ()
run delay f = do
    let i = round $ (10 ** 6) * delay
    threadDelay i
    f
