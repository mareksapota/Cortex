{-# LANGUAGE ScopedTypeVariables #-}

module Adria.Backend.Server
    ( runServer
    ) where

-----

import Prelude hiding (getLine)
import Network
import System.IO
    ( stderr
    , Handle
    , BufferMode (LineBuffering)
    , IOMode (ReadMode, WriteMode)
    )
import System.Cmd (rawSystem)
import Control.Monad.State
import Control.Monad.Error
import Control.Concurrent.Lifted
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)
import Data.Maybe (fromMaybe, isJust, fromJust)

import Adria.Common.ErrorIO
import Adria.Common.Event
import Adria.Backend.Commit (Commit)
import qualified Adria.Backend.Commit as C
import Adria.Backend.ValueStorage (ValueStorage)
import qualified Adria.Backend.Storage as S
import qualified Adria.Common.Random as Random
import qualified Adria.Common.GZip as GZip
import qualified Adria.Common.Middleware as M

import qualified Adria.Config as Config

-----

type SEI a = StateT (MVar ValueStorage, String, Int) (ErrorT String IO) a
type SSEI a = StateT (Handle, String, Int) (StateT (MVar ValueStorage, String, Int) (ErrorT String IO)) a

-----

runServer :: Int -> Maybe String -> SEI ()
runServer serverPort storage = do
    when (isJust storage) $ do
        let s = fromJust storage
        readValueStorage s
        periodicTimer Config.storageTime (saveValueStorage s)
    socket <- iListenOn $ PortNumber $ fromIntegral serverPort
    printLocalLog $ "Server started on port " ++ (show serverPort)
    -- Start sync daemon.
    periodicTimer Config.syncTime (sync serverPort)
    -- Get input from Middleware.
    fork $ middlewareWatch
    forever $ catchError (acceptConnection socket) reportError
    where
        acceptConnection :: Socket -> SEI ()
        acceptConnection socket = do
            (hdl, host, port) <- iAccept socket
            fork $ evalStateT handleConnection (hdl, host, read $ show port)
            -- Ignore result of fork and return proper type.
            return ()

        reportError :: String -> SEI ()
        reportError e = do
            -- If this operation throws an error, accept loop will exit.
            printLocalLog $ "Error: " ++ e

-----

-- `fork` discards errors, so `reportError` from `runServer` won't report them,
-- they have to be caught here.
handleConnection :: SSEI ()
handleConnection = catchError handleConnection' reportError
    where
        reportError :: String -> SSEI ()
        reportError e = printLog $ "Error: " ++ e

handleConnection' :: SSEI ()
handleConnection' = do
    (hdl, _, _) <- get
    iSetBuffering hdl LineBuffering
    getLine >>= chooseConnectionMode

-----

chooseConnectionMode :: String -> SSEI ()

chooseConnectionMode "set" = do
    key <- getLine
    value <- getLine
    printLog $ "set " ++ key
    lift $ S.set key value
    closeConnection

chooseConnectionMode "get" = do
    key <- getLine
    printLog $ "get " ++ key
    value <- lift $ S.lookup key
    putLine $ show value
    closeConnection

chooseConnectionMode "delete" = do
    key <- getLine
    printLog $ "delete " ++ key
    lift $ S.delete key
    closeConnection

chooseConnectionMode "sync" = do
    printLog "sync request"
    host <- getLine
    clientSync
    -- If remote host was assumed offline, client side synchronisation will not
    -- mark it online.
    let key = "host::availability::" ++ host
    remote <- lift $ S.lookup key
    when ("offline" == fromMaybe "offline" remote) $
        lift $ S.set ("host::availability::" ++ host) "online"
    printLog "sync request done"

chooseConnectionMode _ = throwError "Unknown connection mode"

clientSync :: SSEI ()
clientSync = do
    hash <- getLine
    when (hash /= "done") $ do
        member <- lift $ S.member hash
        if member
            then putLine "yes"
            else do
                putLine "no"
                commit <- getLine
                lift $ S.insert $ read commit
                clientSync

-----

sync :: Int -> SEI ()
sync port = do
    printLocalLog "Sync operation started"
    -- TODO: remove, for debug only
    x <- S.lookupAll ""
    printLocalLog $ show x
    --
    let selfHost = concat [Config.host, ":", show port]
    let key = "host::availability::" ++ selfHost
    self <- S.lookup key
    when ("offline" == fromMaybe "offline" self) $ S.set key "online"
    hosts' <- S.lookupAllWhere "host::availability"
        (\k v -> k /= concat [Config.host, ":", show port] && v == "online")
    let hosts = fst $ unzip hosts'
    r <- Random.generate (0, (length hosts) - 1) Config.syncServers
    let syncHosts = map (\i -> hosts !! i) r
    forM_ syncHosts (performSync selfHost)
    printLocalLog "Sync operation finished"

performSync :: String -> String -> SEI ()
performSync selfHost hostString = do
    printLocalLog $ "Synchronising with " ++ hostString
    let host = takeWhile (/= ':') hostString
    let (port :: Int) = read $ tail $ dropWhile (/= ':') hostString
    do
        { hdl <- iConnectTo host port
        ; evalStateT (performSync' selfHost) (hdl, host, port)
        } `catchError` reportError
    printLocalLog $ "Synchronisation with " ++ hostString ++ " done"
    where
        reportError :: String -> SEI ()
        reportError e = do
            printLocalLog $ "Error: " ++ e
            S.set ("host::availability::" ++ hostString) "offline"

performSync' :: String -> SSEI ()
performSync' selfHost = do
    putLine "sync"
    putLine selfHost
    commits <- lift S.getCommits
    performSync'' (reverse commits)
    closeConnection

performSync'' :: [Commit] -> SSEI ()
performSync'' [] = putLine "done"

performSync'' (c:commits) = do
    putLine $ C.getHash c
    l <- getLine
    when (l == "no") $ do
        putLine $ show c
        performSync'' commits

-----

readValueStorage :: String -> SEI ()
readValueStorage location = do
    { printLocalLog $ "Reading storage from " ++ location
    ; hdl <- iOpen location ReadMode
    ; vs <- ibGetContents hdl
    ; iClose hdl
    ; S.read $ GZip.unpack vs
    ; printLocalLog $ "Storage was successfully read"
    } `catchError` reportError
    where
        reportError :: String -> SEI ()
        reportError e = do
            printLocalLog $ "Couldn't read storage: " ++ e

-----

saveValueStorage :: String -> SEI ()
saveValueStorage location = do
    { let tmp = location ++ ".tmp"
    ; hdl <- iOpen tmp WriteMode
    ; vs <- S.show
    ; ibPutStr hdl (GZip.pack vs)
    ; iClose hdl
    ; liftIO $ rawSystem "mv" [tmp, location]
    ; printLocalLog $ "Saved storage to " ++ location
    } `catchError` reportError
    where
        reportError :: String -> SEI ()
        reportError e = do
            printLocalLog $ "Saving storage failed: " ++ e

-----

middlewareWatch :: SEI ()
middlewareWatch = do
    { (host, port) <- S.getMiddlewareHost
    ; M.subscribe host port
        [ "storage::set"
        , "storage::delete"
        , "storage::lookup"
        ]
        handler
    } `catchError` reportError
    where
        reportError :: String -> SEI ()
        reportError e = do
            printLocalLog $ "Middleware watch connection error: " ++ e

        handler :: [String] -> String -> SEI ()
        handler ["storage::set"] msg = do
            let (key, value) = split msg
            S.silentSet key value
        handler ["storage::delete"] msg = S.silentDelete msg
        handler ["storage::lookup"] msg = return ()
        handler _ _ = throwError "This shouldn't have happened"

        split :: String -> (String, String)
        split msg = (takeWhile (/= ' ') msg, tail $ dropWhile (/= ' ') msg)

-----

getLine :: SSEI String
getLine = do
    (hdl, _, _) <- get
    iGetLine hdl

-----

putLine :: String -> SSEI ()
putLine s = do
    (hdl, _, _) <- get
    iPutStrLn hdl s
    iFlush hdl

-----

getHost :: SSEI String
getHost = do
    (_, host, port) <- get
    return $ concat [host, ":", show port]

-----

closeConnection :: SSEI ()
closeConnection = do
    (hdl, _, _) <- get
    iClose hdl

-----

printLog :: String -> SSEI ()
printLog msg = do
    timeString <- currentTime
    host <- getHost
    iPutStrLn stderr $ concat [timeString, " -- ", host, " -- ", msg]
    iFlush stderr

-----

printLocalLog :: String -> SEI ()
printLocalLog msg = do
    timeString <- currentTime
    iPutStrLn stderr $ timeString ++ " -- " ++ msg
    iFlush stderr

-----

currentTime :: (MonadIO m) => m String
currentTime = do
    t <- liftIO getCurrentTime
    return $ formatTime defaultTimeLocale "%m/%d/%Y %H:%M:%S:%q" t
