{-# LANGUAGE ScopedTypeVariables #-}

module Cortex.G23.Alive (runAlive) where

import Control.Concurrent.Lifted (fork, threadDelay)
import System.IO (stderr)
import Control.Monad (liftM, forM_, when)
import Control.Monad.State (evalStateT, get)
import Control.Monad.Error (catchError, throwError)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (isNothing, fromJust)

import Cortex.G23.GrandMonadStack
import qualified Cortex.G23.Config as Config
import Cortex.Common.ErrorIO
import Cortex.Common.Event
import Cortex.Common.MaybeRead

-----

runAlive :: String -> Int -> LesserMonadStack ()
runAlive host port = evalStateT runAlive' (host, port)

runAlive' :: GrandMonadStack ()
runAlive' = do
    periodicTimer Config.aliveCheckTime aliveCheck
    eventLoop

-----

aliveCheck :: GrandMonadStack ()
aliveCheck = do
    { (host, port) <- get
    ; hdl <- iConnectTo host port
    ; iPutStrLn hdl "lookup all"
    ; iPutStrLn hdl "app::instance"
    ; iFlush hdl
    ; instances <- (liftM BS.lines) $ ibGetContents hdl
    ; forM_ instances (\i -> fork $ checkInstance i)
    } `catchError` reportError

-----

checkInstance :: ByteString -> GrandMonadStack ()
checkInstance i = do
    -- Remove the 'app::' prefix.
    { let hp = BS.unpack (BS.drop 2 $ BS.dropWhile (/= ':') i)
    ; let host = takeWhile (/= ':') hp
    ; let (port' :: Maybe Int) = maybeRead $ tail $ dropWhile (/= ':') hp
    ; when (isNothing port') $ throwError "Malformed host:port line"
    ; let port = fromJust port'
    ; catchError (connect host port) (\_ -> remove $ BS.unpack i)
    } `catchError` reportError

-----

connect :: String -> Int -> GrandMonadStack ()
connect host port = do
    -- Try not to kill new instances that aren't properly up yet.
    { threadDelay $ round $ (10 ** 6) * Config.connectionSleep
    ; hdl <- iConnectTo host port
    ; iClose hdl
    }

-----

remove :: String -> GrandMonadStack ()
remove i = do
    { let key = "app::instance::" ++ i
    ; (host, port) <- get
    ; hdl <- iConnectTo host port
    ; iPutStrLn hdl "lookup"
    ; iPutStrLn hdl key
    ; iFlush hdl
    ; response <- iGetLine hdl
    ; iClose hdl
    -- Remove and notify about it only once.
    ; when (response /= "Nothing") $ remove' key
    }

remove' :: String -> GrandMonadStack ()
remove' key = do
    { (host, port) <- get
    ; hdl <- iConnectTo host port
    ; iPutStrLn hdl "delete"
    ; iPutStrLn hdl key
    ; iFlush hdl
    ; iClose hdl
    ; iPutStrLn stderr $ "Detected a dead instance: " ++ key
    }

-----

reportError :: String -> GrandMonadStack ()
reportError e = iPutStrLn stderr $ "Error: " ++ e

-----
