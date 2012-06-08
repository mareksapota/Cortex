{-# LANGUAGE FlexibleContexts #-}

module Cortex.Common.Time
    ( getBigEndianTimestamp
    , getBigEndianMinTimestamp
    , getDefaultTimestamp
    , getEpochTime
    ) where

import qualified Data.Time.Format as F
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Locale (defaultTimeLocale)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Error (MonadError)

import Cortex.Common.IOReport

-----

getBigEndianTimestamp :: (MonadIO m, MonadError String m) => m String
getBigEndianTimestamp = formatTime"%Y.%m.%d %H:%M:%S:%q"

-----

getBigEndianMinTimestamp :: (Monad m) => m String
getBigEndianMinTimestamp = return "0000.00.00 00:00:00:000000000000"

-----

getDefaultTimestamp :: (MonadIO m, MonadError String m) => m String
getDefaultTimestamp = formatTime "%m/%d/%Y %H:%M:%S:%q"

-----

formatTime :: (MonadIO m, MonadError String m) => String -> m String
formatTime format = do
    time <- ioReport getCurrentTime
    return $ F.formatTime defaultTimeLocale format time

-----

getEpochTime :: (MonadIO m, MonadError String m) => m Int
getEpochTime = do
    time <- ioReport getPOSIXTime
    return $ round $ (read $ init $ show time :: Double)

-----
