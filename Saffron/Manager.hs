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
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO (stderr)

import Cortex.Common.Event
import Cortex.Common.ErrorIO
import Cortex.Common.Error
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
    -- Make sure there is load info before running `updateManager`.
    updateLoad
    periodicTimer Config.managerUpdateTime updateManager
    -- Make sure errors don't stop the timer.
    periodicTimer Config.managerUpdateTime $
        updateLoad `catchError` (\_ -> return ())
    eventLoop

-----
-- This function doesn't generate errors.  Two instances of this function should
-- not run simultaneously.

updateManager :: ManagerMonadStack ()
updateManager = do
    { iPutStrLn stderr "Looking for new apps"
    ; (host, port, _) <- get
    ; hdl <- iConnectTo host port
    ; iPutStrLn hdl "lookup all"
    ; iPutStrLn hdl "app::type"
    ; iFlush hdl
    ; apps <- ibGetContents hdl
    ; forM_ (BS.lines apps) tryAddApp
    } `catchError` reportError
    where
        reportError :: String -> ManagerMonadStack ()
        reportError e = iPutStrLn stderr $ "Error: " ++ e

        tryAddApp :: ByteString -> ManagerMonadStack ()
        tryAddApp app' = do
            let app = BS.unpack app'
            apps <- getApps
            when (not (Set.member app apps)) $ do
                addApp app
                fork $ (runAppManager app) `finally` do
                    { iPutStrLn stderr $ "App manager exited: " ++ app
                    ; removeApp app
                    }
                return ()

-----
-- Update CPU load of this host.

updateLoad :: ManagerMonadStack ()
updateLoad = do
    { load <- iReadProcess "Saffron/CPULoad.py" []
    ; (host, port, _) <- get
    ; hdl <- iConnectTo host port
    ; iPutStrLn hdl "set"
    ; iPutStrLn hdl $ concat ["host::load::", Config.host]
    ; iPutStrLn hdl load
    ; iFlush hdl
    ; iClose hdl
    }

-----

getApps :: ManagerMonadStack (Set String)
getApps = do
    (_, _, mv) <- get
    apps <- readMVar mv
    return apps

-----

addApp :: String -> ManagerMonadStack ()
addApp app = do
    (_, _, mv) <- get
    apps <- takeMVar mv
    putMVar mv $ Set.insert app apps

-----

removeApp :: String -> ManagerMonadStack ()
removeApp app = do
    (_, _, mv) <- get
    apps <- takeMVar mv
    putMVar mv $ Set.delete app apps

-----
