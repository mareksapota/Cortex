{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Cortex.Common.Miranda
    ( newMirandaInfo
    , mirandaConnect
    , mirandaUpdateInfo
    , getWorkingMirandaInfo
    , MirandaInfo
    ) where

-----

import Control.Concurrent.Lifted
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Error (MonadError, catchError, throwError)
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (isNothing, fromJust)

import Cortex.Common.LazyIO
import Cortex.Common.ErrorIO
import Cortex.Common.MaybeRead
import Cortex.Common.Event
import Cortex.Common.Error

-----

type HostName = String
type PortNumber = Int
type HostInfo = (HostName, PortNumber)
-- Main Miranda instance and backup servers.
type MirandaInfo = MVar (HostInfo, [HostInfo])

-----

newMirandaInfo :: (MonadBase IO m) => HostName -> PortNumber ->
    m MirandaInfo
newMirandaInfo h p = newMVar ((h, p), [])

-----
-- Returns host and port of a working Miranda instance.

getWorkingMirandaInfo :: (MonadError String m, MonadBase IO m, MonadIO m) =>
    MirandaInfo -> m HostInfo
getWorkingMirandaInfo mv = do
    ((host, port), backup) <- readMVar mv
    tryConnect $ (host, port):backup
    where
        tryConnect [] = throwError "No Miranda instances are available."
        tryConnect ((host, port):t) = do
            { hdl <- iConnectTo host port
            ; lClose hdl
            ; return (host, port)
            } `catchError` (\_ -> tryConnect t)

-----

mirandaConnect :: (MonadError String m, MonadBase IO m, MonadIO m) =>
    MirandaInfo -> m LazyHandle
mirandaConnect mv = do
    ((host, port), backup) <- readMVar mv
    iConnectTo host port `catchError` (\e -> do
        { iPrintLog "Main Miranda instance is down, using backup servers."
        ; connectBackup backup e
        })
    where
        connectBackup [] e = throwError e
        connectBackup ((host, port):t) _ =
            iConnectTo host port `catchError` (\e -> connectBackup t e)

-----

mirandaUpdateInfo :: (MonadError String m, MonadBase IO m, MonadIO m,
    MonadBaseControl IO m) => MirandaInfo -> m TimerHandle
mirandaUpdateInfo mv = periodicTimer 5.0 $ ignoreError $ do
    { hdl <- mirandaConnect mv
    ; lPutStrLn hdl "lookup all with value"
    ; lPutStrLn hdl "host::availability"
    ; lFlush hdl
    ; hosts <- liftM (takeOnline . group) $ lGetLines hdl
    ; lClose hdl
    ; let newInfo = mkInfo hosts
    ; ((host, port), _) <- takeMVar mv
    ; putMVar mv ((host, port), newInfo)
    }
    where
        group :: [a] -> [(a, a)]
        group l = group' l []

        group' [] l = l
        group' (h1:h2:t) l = group' t ((h1, h2):l)
        -- Odd number of list elements, this shouldn't happen.
        group' [_] l = group' [] l

        takeOnline :: [(LBS.ByteString, LBS.ByteString)] -> [String]
        takeOnline l = takeOnline' l []

        takeOnline' [] l = l
        takeOnline' ((h, o):t) l
            | o == "online" = takeOnline' t ((LBS.unpack h):l)
            | otherwise = takeOnline' t l

        mkInfo :: [String] -> [HostInfo]
        mkInfo l = mkInfo' l []

        mkInfo' [] l = l
        mkInfo' (h:t) l = do
            let host = takeWhile (':' /=) h
            let port = maybeRead $ tail $ dropWhile (':' /=) h :: Maybe Int
            if isNothing port
                then mkInfo' t l
                else mkInfo' t ((host, fromJust port):l)

-----
