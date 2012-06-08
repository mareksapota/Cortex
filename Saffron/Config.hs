module Cortex.Saffron.Config where

host :: String
host = "127.0.0.1"

managerUpdateTime :: Double
managerUpdateTime = 1.0

instanceDeleteTime :: Double
instanceDeleteTime = 10.0

-- Hosts that haven't updated their load in this many seconds will be considered
-- dead.
oldLoadTime :: Int
oldLoadTime = 5

-- If running instances + `almostFull` >= requested instances, Saffron will slow
-- down instance spawning so there is less probability of going over the limit
-- (if we go over the limit then all app managers will want to kill an instance,
-- then they will start new instances again and it can go on forever).
almostFull :: Int
almostFull = 2

-- Slow down to x * normal speed if we are almost full.
almostFullSlowdown :: Double
almostFullSlowdown = 0.25
