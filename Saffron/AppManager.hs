{-# LANGUAGE FlexibleContexts #-}

module Cortex.Saffron.AppManager
    ( runAppManager
    ) where

-----

import Control.Monad.Base (MonadBase)
import Control.Monad.State (get, evalStateT)
import Control.Monad.Error (throwError, catchError)
import Control.Monad (forM_, forM, when, liftM)
import Control.Monad.Trans (lift, liftIO)
import System.IO (stderr)
import Control.Concurrent.Lifted hiding (killThread)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO.Temp (createTempDirectory)
import Data.Maybe (catMaybes, fromJust, isNothing, isJust)
import System.Random (randomRIO)

import Cortex.Saffron.GrandMonadStack
import Cortex.Common.ErrorIO
import Cortex.Common.Error
import Cortex.Common.Event
import Cortex.Common.MaybeRead
import qualified Cortex.Saffron.Config as Config

import qualified Cortex.Saffron.Apps.Static as AppStatic

-----

knownAppTypes :: Map String
    ( String -> AppManagerMonadStack ()
    , Int -> String -> AppManagerMonadStack (MVar (), MVar Int)
    )

knownAppTypes = Map.fromList
    [ ("static", (AppStatic.prepare, AppStatic.run))
    ]

-----

runAppManager :: String -> ManagerMonadStack ()
runAppManager app = do
    (host, port, _) <- get
    a <- newMVar []
    b <- newMVar ""
    c <- newMVar ""
    d <- newMVar Nothing
    mv <- newMVar (a, b, c, d)
    lift $ evalStateT runAppManager' (host, port, app, mv)

runAppManager' :: AppManagerMonadStack ()
runAppManager' = do
    { (_, _, app, mv) <- get
    ; iPutStrLn stderr $ "Running app manager for a new app: " ++ app
    ; lock <- newEmptyMVar
    ; t1 <- addTimer lock Config.managerUpdateTime checkSource
    ; t2 <- addTimer lock Config.managerUpdateTime addInstance
    -- Don't kill too often, let the CPU use drop between kills.
    ; t3 <- addTimer lock Config.instanceDeleteTime killInstance
    ; t4 <- addTimer lock Config.instanceDeleteTime cleanInstances
    ; takeMVar lock
    ; stopTimer t1
    ; stopTimer t2
    ; stopTimer t3
    ; stopTimer t4
    ; (threads, _, _, location) <- takeMVar mv
    ; ignoreError (killAllThreads threads)
    ; ignoreError (removeLocation location)
    } `catchError` (\e -> iPutStrLn stderr $ "Error: " ++ e)

-----
-- WARNING: Make sure that timers leave consistent state even if some operation
-- throws errors.  Dirty state is also a consistent state.

addTimer :: MVar () -> Double ->
    (AppManagerState -> AppManagerMonadStack ()) ->
    AppManagerMonadStack TimerHandle
addTimer lock t s = periodicTimer t $ do
    (_, _, _, mv) <- get
    -- Take the big lock, make sure only one timer is running.
    (a, b, c, d) <- takeMVar mv
    isDirty <- dirty a b c d
    if isDirty
        then putMVar mv (a, b, c, d)
        else do
            (s (a, b, c, d)) `catchError` reportError
            -- Only panic on serious errors.
            isDirty' <- dirty a b c d
            when (isDirty') $ tryPutMVar lock () >> return ()
            putMVar mv (a, b, c, d)
    where
        dirty :: MonadBase IO m => MVar a -> MVar b -> MVar c -> MVar d ->
            m Bool
        dirty a b c d = do
            a' <- isEmptyMVar a
            b' <- isEmptyMVar b
            c' <- isEmptyMVar c
            d' <- isEmptyMVar d
            return $ or [a', b', c', d']

-----

checkSource :: AppManagerState -> AppManagerMonadStack ()
checkSource (threads, appType, sourceHash, location) = do
    { newAppType <- liftM BS.unpack $ getProperty "type"
    ; newSourceHash <- getPropertyHash "source"
    ; appType' <- readMVar appType
    ; sourceHash' <- readMVar sourceHash
    ; when ((appType' /= newAppType) || (sourceHash' /= newSourceHash)) $ do
        { (host, port, app, _) <- get
        ; iPutStrLn stderr $ "Reloading app manager: " ++ app
        ; killAllThreads threads
        ; removeLocation location
        ; when (not $ Map.member newAppType knownAppTypes) $
            throwError $ "Unsupported app type: " ++ newAppType
        ; newLocation <- makeNewLocation location
        ; takeMVar appType
        ; takeMVar sourceHash
        -- If this operation throws an error, the state will still be consistent
        -- (although it will be dirty) since app type and source hash will be
        -- missing.
        ; iRawSystem "Saffron/GetSource.py"
            [ host
            , show port
            , app
            , newLocation
            ]
        ; (fst (knownAppTypes Map.! newAppType)) newLocation
        ; putMVar appType newAppType
        ; putMVar sourceHash newSourceHash
        }
    }

-----
-- (running instances -> requested instances -> should act?).

cmpInstances :: (Int -> Int -> Bool) -> AppManagerMonadStack Bool
cmpInstances f = do
    (host, port, app, _) <- get
    hdl <- iConnectTo host port
    iPutStrLn hdl "lookup all"
    iPutStrLn hdl $ "app::instance::" ++ app
    iFlush hdl
    running <- (liftM $ length . BS.lines) $ ibGetContents hdl
    requested <- (liftM $ maybeRead . BS.unpack) $ getProperty "instances"
    when (isNothing requested) $ throwError "Expected a numeric value"
    return $ f running (fromJust requested)

-----
-- (self load -> average load of all hosts -> should act?).

cmpLoad :: (Double -> Double -> Bool) -> AppManagerMonadStack Bool
cmpLoad f = do
    load <- getLoad
    return (uncurry f $ load)

-----

addInstance :: AppManagerState -> AppManagerMonadStack ()
addInstance (threads, appType, _, location) = do
    act1 <- cmpInstances (<)
    act2 <- cmpLoad (\a b -> a - b < 0.1)
    -- Can't run if there is no source.
    location' <- readMVar location
    let act3 = isJust location'
    when (act1 && act2 && act3) $ do
        (h, p, app, _) <- get
        appType' <- readMVar appType
        -- Pick a random port, if it's taken then this instance will get killed,
        -- but it's OK, another one will take it's place.
        port <- liftIO $ randomRIO (1025, 65535)
        iPutStrLn stderr $ "Running a new instance of " ++ app ++
            " on port " ++ (show port)
        a <- (snd (knownAppTypes Map.! appType')) port (fromJust location')
        addThread a threads
        -- Notify Miranda.
        hdl <- iConnectTo h p
        iPutStrLn hdl "set"
        iPutStrLn hdl $ concat
            [ "app::instance::"
            , app
            , "::"
            , Config.host
            , ":"
            , show port
            ]
        iPutStrLn hdl "online"
        iClose hdl

-----

killInstance :: AppManagerState -> AppManagerMonadStack ()
killInstance (threads, _, _, _) = do
    act1 <- cmpLoad (\a b -> a - b > 0.2)
    act2 <- cmpInstances (>)
    -- Overloaded node (let someone else run an instance), or too much instances
    -- running.
    when (act1 || act2) $ do
        (_, _, app, _) <- get
        iPutStrLn stderr $ "Killing an instance of " ++ app
        killOneThread threads

-----
-- Remove dead instances from list.  Instance can die if server subprocess died
-- or someone remove info about this instance from Miranda.  Use this only under
-- the big lock.

cleanInstances :: AppManagerState -> AppManagerMonadStack ()
cleanInstances (mv, _, _, _) = do
    dirty <- isEmptyMVar mv
    when dirty $ throwError "Dirty state encountered"
    threads <- takeMVar mv
    threads' <- cleanInstances' threads []
    putMVar mv threads'

cleanInstances' :: [(MVar (), MVar Int)] -> [(MVar (), MVar Int)] ->
    AppManagerMonadStack [(MVar (), MVar Int)]
cleanInstances' [] l = return l
cleanInstances' (t:rest) l = do
    alive <- isEmptyMVar (snd t)
    if alive
        then cleanInstances' rest (t:l)
        else do
            -- Notify Miranda about this dead instance.
            killThread t
            cleanInstances' rest l

-----
-- Self load and average load of all hosts.

getLoad :: AppManagerMonadStack (Double, Double)
getLoad = do
    { (host, port, _, _) <- get
    ; hdl <- iConnectTo host port
    ; iPutStrLn hdl "lookup all"
    ; iPutStrLn hdl "host::load"
    ; iFlush hdl
    ; hosts' <- (liftM $ map BS.unpack . BS.lines) $ ibGetContents hdl
    ; let hosts = map ("host::load::" ++) hosts'
    ; values' <- forM hosts getValue
    -- Some keys may have been unset between the `lookup all` call and now,
    -- ignore them and use only the set values.
    ; let values = catMaybes values' :: [Double]
    ; let avg = if (null values)
            then 0.0
            else (sum values) / (fromIntegral $ length values)
    ; self <- (liftM maybeRead) $ iReadProcess "Saffron/Load.py" []
    ; when (isNothing self) $ throwError "Load.py didn't return a Double"
    ; return (fromJust self, avg)
    }
    where
        getValue :: MaybeRead a => String -> AppManagerMonadStack (Maybe a)
        getValue key = do
            (host, port, _, _) <- get
            hdl <- iConnectTo host port
            iPutStrLn hdl "lookup"
            iPutStrLn hdl key
            iFlush hdl
            v' <- (liftM $ BS.takeWhile (/= '\n')) $ ibGetContents hdl
            let v = BS.unpack v'
            if (v == "Nothing")
                then return Nothing
                else return $ maybeRead (drop 5 v)

-----
-- Use this only under the big lock.

removeLocation :: MVar (Maybe String) -> AppManagerMonadStack ()
removeLocation mv = do
    dirty <- isEmptyMVar mv
    when dirty $ throwError "Dirty state encountered"
    location <- takeMVar mv
    removeLocation' location
    putMVar mv Nothing

removeLocation' :: Maybe String -> AppManagerMonadStack ()
removeLocation' Nothing = return ()
removeLocation' (Just location) = iRawSystem "rm" ["-rf", location]

-----
-- Use this only under the big lock.
--
-- TODO: Location is never removed if Saffron is killed by a signal.

makeNewLocation :: MVar (Maybe String) -> AppManagerMonadStack String
makeNewLocation mv = do
    dirty <- isEmptyMVar mv
    when dirty $ throwError "Dirty state encountered"
    location <- takeMVar mv
    when (isJust location) $ throwError "Location is not empty"
    (_, _, app, _) <- get
    dir <- liftIO $ createTempDirectory "/tmp" (app ++ ".source.")
    putMVar mv (Just dir)
    return dir

-----
-- Kill all instances.  Use this only under the big lock.

killAllThreads :: MVar [(MVar (), MVar Int)] -> AppManagerMonadStack ()
killAllThreads mv = do
    dirty <- isEmptyMVar mv
    when dirty $ throwError "Dirty state encountered"
    threads <- takeMVar mv
    forM_ threads killThread
    putMVar mv []

-----
-- Kill one instance.  Use this only under the big lock.

killOneThread :: MVar [(MVar (), MVar Int)] -> AppManagerMonadStack ()
killOneThread mv = do
    dirty <- isEmptyMVar mv
    when dirty $ throwError "Dirty state encountered"
    threads <- takeMVar mv
    threads' <- killOneThread' threads
    putMVar mv threads'

killOneThread' :: [(MVar (), MVar Int)] ->
    AppManagerMonadStack [(MVar (), MVar Int)]
killOneThread' [] = return []
killOneThread' (t:rest) = do
    alive <- isEmptyMVar (snd t)
    -- Notify Miranda even when the thread is dead.
    killThread t
    if alive
        then return rest
        -- This thread was already dead.
        else killOneThread' rest

-----

killThread :: (MVar (), MVar Int) -> AppManagerMonadStack ()
killThread t = do
    -- Use non blocking version in case something else killed this thread.
    tryPutMVar (fst t) ()
    port <- takeMVar (snd t)

    -- Notify Miranda.
    (h, p, app, _) <- get
    hdl <- iConnectTo h p
    iPutStrLn hdl "delete"
    iPutStrLn hdl $ concat
        [ "app::instance::"
        , app
        , "::"
        , Config.host
        , ":"
        , show port
        ]
    iClose hdl

    iPutStrLn stderr $ concat
        [ "Instance of "
        , app
        , " running on port "
        , show port
        , " is dead"
        ]

-----
-- Use this only under the big lock.

addThread :: (MVar (), MVar Int) -> MVar [(MVar (), MVar Int)] ->
    AppManagerMonadStack ()
addThread t mv = do
    dirty <- isEmptyMVar mv
    when dirty $ throwError "Dirty state encountered"
    threads <- takeMVar mv
    putMVar mv (t:threads)

-----

getProperty :: String -> AppManagerMonadStack ByteString
getProperty property = do
    value <- getPropertyCommon property "lookup"
    when ((BS.take 7 value) == (BS.pack "Nothing")) $
        throwError "Property is not set"
    return $ BS.drop 5 value

-----

getPropertyHash :: String -> AppManagerMonadStack String
getPropertyHash property = do
    hash <- liftM BS.unpack $ getPropertyCommon property "lookup hash"
    when (hash == "Nothing") $ throwError "Property is not set"
    return $ drop 5 hash

-----

getPropertyCommon :: String -> String -> AppManagerMonadStack ByteString
getPropertyCommon property command = do
    (host, port, app, _) <- get
    let key = concat ["app::", property, "::", app]
    hdl <- iConnectTo host port
    iPutStrLn hdl command
    iPutStrLn hdl key
    iFlush hdl
    bs <- ibGetContents hdl
    return $ BS.takeWhile (/= '\n') bs

-----
