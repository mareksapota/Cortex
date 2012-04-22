module Cortex.Common.Time
    ( getBigEndianTimestamp
    , getBigEndianMinTimestamp
    , getDefaultTimestamp
    ) where

import qualified Data.Time.Format as F
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)
import Control.Monad.Trans (MonadIO, liftIO)

-----

getBigEndianTimestamp :: (MonadIO m) => m String
getBigEndianTimestamp = formatTime"%Y.%m.%d %H:%M:%S:%q"

-----

getBigEndianMinTimestamp :: (Monad m) => m String
getBigEndianMinTimestamp = return "0000.00.00 00:00:00:000000000000"

-----

getDefaultTimestamp :: (MonadIO m) => m String
getDefaultTimestamp = formatTime "%m/%d/%Y %H:%M:%S:%q"

-----

formatTime :: (MonadIO m) => String -> m String
formatTime format = do
    time <- liftIO getCurrentTime
    return $ F.formatTime defaultTimeLocale format time

-----
