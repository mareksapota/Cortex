{-# LANGUAGE FlexibleContexts #-}

module Cortex.Common.Event
    ( timer
    , periodicTimer
    , stopTimer
    , eventLoop
    , TimerHandle
    ) where

import System.Random
import Control.Monad.Base
import Control.Concurrent.Lifted
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Control
import Control.Monad (forever, liftM)
import Data.Maybe (isNothing)

-----

-- Should stop, is stopped.
type TimerHandle = MVar ()

-----

timer :: (MonadBaseControl IO m, MonadBase IO m) =>
    Double -> m () -> m ()
timer delay f = do
    fork $ run delay f
    return ()

-----
-- Add a periodic timer that will run some code every `delay` seconds.  Two
-- instances of this code won't be run simultaneously.  Returns a timer handle
-- that can be used to stop the timer.

periodicTimer :: (MonadBaseControl IO m, MonadBase IO m, MonadIO m) =>
    Double -> m () -> m TimerHandle
periodicTimer delay f = do
    hdl <- newEmptyMVar
    fork $ do
        -- Start somewhere between `0.0` and `delay` seconds.
        t <- liftIO $ randomRIO (0.0, delay)
        threadDelay $ round $ (10 ** 6) * t
        -- First call.
        f
        -- Start the timer loop.
        loop hdl
    return hdl
    where
        loop hdl = do
            again <- (liftM isNothing) $ tryTakeMVar hdl
            if again
                then do
                    run delay f
                    loop hdl
                else return ()

-----
-- Prevents a periodic timer from making more iterations.  This function will
-- wait until the periodic funciton has finished - when this function exits, you
-- can be certain that the timer is not running and won't be running again.
-- Because this is a blocking call you should make sure that the periodic
-- function will exit, if it calls `forever` or blocks on an `MVar` or something
-- this function will never exit.

stopTimer :: (MonadBase IO m) => TimerHandle -> m ()
stopTimer hdl = do
    putMVar hdl ()
    -- Wait until timer exits.
    putMVar hdl ()

-----
-- Infinite loop of sleeps.

eventLoop :: (MonadBase IO m) => m ()
eventLoop = do
    -- Sleep for 1m in a loop.
    let sleepTime = round $ ((10 ** 6) * 60.0 :: Double)
    forever $ threadDelay sleepTime

-----

run :: (MonadBase IO m) => Double -> m () -> m ()
run delay f = do
    let i = round $ (10 ** 6) * delay
    threadDelay i
    f

-----
