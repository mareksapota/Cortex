module Cortex.Miranda.Config where

host :: String
host = "127.0.0.1"

-- How often should Miranda perform a sync.
syncTime :: Double
syncTime = 1.0

-- How many servers shoud be contacted during each sync.
syncServers :: Int
syncServers = 2

-- How often should this node initiate a commit squash?  This value should be
-- either reverse proportional to the number of participating Miranda instances
-- or should be set to `0.0` on slave instances (only master instances will be
-- able to initiate a squash then).  This time is not strictly enforced, but the
-- average time between squashes should have a limes in this value.
squashTime :: Double
squashTime = 60.0

-- How often should Miranda backup itself on disc.
storageTime :: Double
storageTime = 60.0

-- Should Miranda print logs to stderr?  Logs will decrease performance.
writeLog :: Bool
writeLog = True
