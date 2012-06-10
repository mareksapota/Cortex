{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Cortex.G23.Alive (runAlive) where

import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Monad (forM_, when)
import Control.Monad.State (evalStateT, get)
import Control.Monad.Error (catchError, throwError)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (isNothing, fromJust)

import Cortex.G23.GrandMonadStack
import qualified Cortex.G23.Config as Config
import Cortex.Common.ErrorIO (iConnectTo, iPrintLog)
import Cortex.Common.LazyIO
import Cortex.Common.Error
import Cortex.Common.Event
import Cortex.Common.MaybeRead
import Cortex.Common.Miranda

-----

runAlive :: String -> Int -> LesserMonadStack ()
runAlive host port = do
    mi <- newMirandaInfo host port
    mirandaUpdateInfo mi
    evalStateT runAlive' mi

runAlive' :: GrandMonadStack ()
runAlive' = do
    periodicTimer Config.aliveCheckTime aliveCheck
    eventLoop

-----

aliveCheck :: GrandMonadStack ()
aliveCheck = do
    { mi <- get
    ; hdl <- mirandaConnect mi
    ; lPutStrLn hdl "lookup all"
    ; lPutStrLn hdl "app::instance"
    ; lFlush hdl
    ; instances <- lGetLines hdl
    ; lClose hdl
    ; forM_ instances (\i -> fork $ checkInstance i)
    } `catchError` reportError

-----

checkInstance :: LBS.ByteString -> GrandMonadStack ()
checkInstance i = do
    -- Remove the 'appName::' prefix.
    { let hp = LBS.drop 2 $ LBS.dropWhile (/= ':') i
    ; let host = LBS.unpack $ LBS.takeWhile (/= ':') hp
    ; let (port' :: Maybe Int) = maybeRead $ LBS.unpack $ LBS.tail $
            LBS.dropWhile (/= ':') hp
    ; when (isNothing port') $ throwError "Malformed host:port line"
    ; let port = fromJust port'
    ; catchError (connect host port) (\_ -> remove i)
    } `catchError` reportError

-----

connect :: String -> Int -> GrandMonadStack ()
connect host port = do
    -- Try not to kill new instances that aren't properly up yet.
    { threadDelay $ round $ (10 ** 6) * Config.connectionSleep
    ; hdl <- iConnectTo host port
    ; lClose hdl
    }

-----

remove :: LBS.ByteString -> GrandMonadStack ()
remove i = do
    { let key = LBS.concat ["app::instance::", i]
    ; mi <- get
    -- Do a lookup first, we don't want to notify the user if the instance is
    -- already dead, for example killed by another G23 thread.
    ; hdl <- mirandaConnect mi
    ; lPutStrLn hdl "lookup"
    ; lPutStrLn hdl key
    ; lFlush hdl
    ; response <- lGetLine hdl
    ; lClose hdl
    -- Don't issue a delete if we don't have to.
    ; when (response /= "Nothing") $ remove' key
    }

remove' :: LBS.ByteString -> GrandMonadStack ()
remove' key = do
    { mi <- get
    ; hdl <- mirandaConnect mi
    ; lPutStrLn hdl "delete"
    ; lPutStrLn hdl key
    ; lFlush hdl
    ; lClose hdl
    ; iPrintLog $ "Detected a dead instance: " ++ (LBS.unpack key)
    }

-----
