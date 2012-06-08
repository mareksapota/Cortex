{-# LANGUAGE OverloadedStrings #-}

module Cortex.Saffron.Manager
    ( runManager
    ) where

-----

import Control.Monad.State (evalStateT, get)
import Control.Monad.Error (catchError)
import Control.Monad (forM_, when)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent.Lifted
import qualified Data.ByteString.Lazy.Char8 as LBS

import Cortex.Common.Event
import Cortex.Common.ErrorIO (iPrintLog, iConnectTo, iReadProcess)
import Cortex.Common.LazyIO
import Cortex.Common.Error
import Cortex.Common.Time
import Cortex.Saffron.GrandMonadStack
import qualified Cortex.Saffron.Config as Config
import Cortex.Saffron.AppManager (runAppManager)

-----

runManager :: String -> Int -> LesserMonadStack ()
runManager host port = do
    s <- newMVar Set.empty
    evalStateT runManager' (host, port, s)

-----

runManager' :: ManagerMonadStack ()
runManager' = do
    periodicTimer Config.managerUpdateTime updateManager
    -- Make sure errors don't stop the timer.
    periodicTimer Config.managerUpdateTime $ ignoreError updateLoad
    eventLoop

-----
-- This function doesn't (or shouldn't =D) generate errors.  Two instances of
-- this function should not run simultaneously.

updateManager :: ManagerMonadStack ()
updateManager = do
    { iPrintLog "Looking for new apps"
    ; (host, port, _) <- get
    ; hdl <- iConnectTo host port
    ; lPutStrLn hdl "lookup all"
    ; lPutStrLn hdl "app::type"
    ; lFlush hdl
    ; apps <- lGetLines hdl
    ; lClose hdl
    ; forM_ apps tryAddApp
    } `catchError` reportError
    where
        tryAddApp :: LBS.ByteString -> ManagerMonadStack ()
        tryAddApp app = do
            apps <- getApps
            when (not (Set.member app apps)) $ do
                addApp app
                fork $ (runAppManager app) `finally` do
                    { iPrintLog $ "App manager exited: " ++ (LBS.unpack app)
                    ; removeApp app
                    }
                return ()

-----
-- Update load of this host.

updateLoad :: ManagerMonadStack ()
updateLoad = do
    { load <- iReadProcess "Saffron/Load.py" []
    ; time <- getEpochTime
    ; (host, port, _) <- get
    ; hdl <- iConnectTo host port
    ; lPutStrLn hdl "set"
    ; lPutStr hdl "host::load::"
    ; lPutStrLn hdl $ LBS.pack Config.host
    ; lPutStr hdl $ LBS.pack $ show time
    ; lPutStr hdl ":"
    ; lPutStrLn hdl $ LBS.pack load
    ; lClose hdl
    }

-----

getApps :: ManagerMonadStack (Set LBS.ByteString)
getApps = do
    (_, _, mv) <- get
    apps <- readMVar mv
    return apps

-----

addApp :: LBS.ByteString -> ManagerMonadStack ()
addApp app = do
    (_, _, mv) <- get
    apps <- takeMVar mv
    putMVar mv $ Set.insert app apps

-----

removeApp :: LBS.ByteString -> ManagerMonadStack ()
removeApp app = do
    (_, _, mv) <- get
    apps <- takeMVar mv
    putMVar mv $ Set.delete app apps

-----
