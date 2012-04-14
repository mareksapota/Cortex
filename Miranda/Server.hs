{-# LANGUAGE ScopedTypeVariables #-}

module Cortex.Miranda.Server
    ( runServer
    ) where

-----

import Prelude hiding (getLine)
import Network
import System.IO
    ( stderr
    , Handle
    , BufferMode (LineBuffering)
    , IOMode (ReadMode, WriteMode, AppendMode)
    )
import System.Cmd (rawSystem)
import Control.Monad.State
import Control.Monad.Error
import Control.Concurrent.Lifted
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Cortex.Common.ErrorIO
import Cortex.Common.Event
import Cortex.Common.Error
import Cortex.Common.MaybeRead
import Cortex.Miranda.Commit (Commit)
import qualified Cortex.Miranda.Commit as C
import qualified Cortex.Miranda.Storage as S
import qualified Cortex.Common.Random as Random
import Cortex.Miranda.GrandMonadStack

import qualified Cortex.Miranda.Config as Config

-----

type ConnectedMonadStack = StateT (Handle, String, Int) GrandMonadStack

-----

runServer :: Int -> GrandMonadStack ()
runServer serverPort = do
    readValueStorage
    periodicTimer Config.storageTime saveValueStorage
    socket <- iListenOn $ PortNumber $ fromIntegral serverPort
    printLocalLog $ "Server started on port " ++ (show serverPort)
    -- Start sync daemon.
    periodicTimer Config.syncTime (sync serverPort)
    forever $ catchError (acceptConnection socket) reportError
    where
        acceptConnection :: Socket -> GrandMonadStack ()
        acceptConnection socket = do
            (hdl, host, port) <- iAccept socket
            fork $ evalStateT handleConnection (hdl, host, read $ show port)
            -- Ignore result of fork and return proper type.
            return ()

-----

-- `fork` discards errors, so `reportError` from `runServer` won't report them,
-- they have to be caught here.
handleConnection :: ConnectedMonadStack ()
handleConnection = catchError handleConnection' reportError

handleConnection' :: ConnectedMonadStack ()
handleConnection' = do
    (hdl, _, _) <- get
    iSetBuffering hdl LineBuffering
    getLine >>= chooseConnectionMode

-----

chooseConnectionMode :: String -> ConnectedMonadStack ()

chooseConnectionMode "set" = do
    key <- getLine
    rest <- getRest
    let value = BS.takeWhile (/= '\n') rest
    printLog $ "set: " ++ key
    lift $ S.set key value

chooseConnectionMode "lookup" = do
    key <- getLine
    printLog $ "lookup: " ++ key
    value <- lift $ S.lookup key
    if (isNothing value)
        then putLine "Nothing"
        else do
            writePart "Just "
            putbLine (fromJust value)
    closeConnection

chooseConnectionMode "lookup all" = do
    key <- getLine
    printLog $ "lookup all: " ++ key
    kv <- lift $ S.lookupAll key
    let keys = fst $ unzip kv
    forM_ keys putLine
    closeConnection

chooseConnectionMode "lookup all with value" = do
    key <- getLine
    printLog $ "lookup all with value: " ++ key
    kv <- lift $ S.lookupAll key
    forM_ kv $ \(k, v) -> do
        { putLine k
        ; putbLine v
        }
    closeConnection

chooseConnectionMode "lookup hash" = do
    key <- getLine
    printLog $ "lookup hash: " ++ key
    hash <- lift $ S.lookupHash key
    if (isNothing hash)
        then putLine "Nothing"
        else do
            writePart "Just "
            putLine (fromJust hash)
    closeConnection

chooseConnectionMode "delete" = do
    key <- getLine
    printLog $ "delete: " ++ key
    lift $ S.delete key
    closeConnection

chooseConnectionMode "sync" = do
    printLog "sync request"
    host <- getLine
    -- If remote host was assumed offline, client side synchronisation will not
    -- mark it online.
    let key = "host::availability::" ++ host
    remote <- lift $ S.lookup key
    let boff = BS.pack "offline"
    when (boff == fromMaybe boff remote) $ do
        lift $ S.set key (BS.pack "online")
        printLog $ concat ["Marked ", host, " online"]
    rest <- getRest
    clientSync (BS.lines rest) 0
    printLog "sync request done"

chooseConnectionMode _ = throwError "Unknown connection mode"

-- Answer questions about present commits.
clientSync :: [ByteString] -> Int -> ConnectedMonadStack ()
clientSync [] _ = throwError "Expected more lines"
clientSync (h:t) n = do
    let hash = BS.unpack h
    member <- lift $ S.member hash
    if member || (hash == "done")
        then do
            putLine "yes"
            clientSync' t n
        else do
            putLine "no"
            clientSync t (n + 1)

-- Collect remote commits.
clientSync' :: [ByteString] -> Int -> ConnectedMonadStack ()
clientSync' _ 0 = return ()
clientSync' [] _ = throwError "Expected more lines"
clientSync' (h:t) n = do
    c <- lift $ C.fromString h
    lift $ S.insert c
    clientSync' t (n - 1)

-----

sync :: Int -> GrandMonadStack ()
sync port = do
    let selfHost = concat [Config.host, ":", show port]
    let key = "host::availability::" ++ selfHost
    self <- id $ S.lookup key
    let boff = BS.pack "offline"
    when (boff == fromMaybe boff self) (S.set key (BS.pack "online"))
    hosts' <- S.lookupAllWhere "host::availability"
        (\k v ->
            k /= concat [Config.host, ":", show port] &&
            v == (BS.pack "online"))
    let hosts = fst $ unzip hosts'
    r <- Random.generate (0, (length hosts) - 1) Config.syncServers
    let syncHosts = map (\i -> hosts !! i) r
    forM_ syncHosts (performSync selfHost)

performSync :: String -> String -> GrandMonadStack ()
performSync selfHost hostString = do
    printLocalLog $ "Synchronising with " ++ hostString
    let host = takeWhile (/= ':') hostString
    let (port' :: Maybe Int) = maybeRead $ tail $ dropWhile (/= ':') hostString
    when (isNothing port') $ throwError "Malformed host:port line"
    let port = fromJust port'
    do
        { hdl <- iConnectTo host port
        ; evalStateT (performSync' selfHost) (hdl, host, port)
        } `catchError` reportSyncError
    printLocalLog $ "Synchronisation with " ++ hostString ++ " done"
    where
        reportSyncError :: String -> GrandMonadStack ()
        reportSyncError e = do
            printLocalLog $ "Error: " ++ e
            printLocalLog $ concat ["Marking ", hostString, " offline"]
            S.set ("host::availability::" ++ hostString) (BS.pack "offline")

-- Commits are first collected, then transmitted so they can be sent in oldest
-- to newest order.  This means they can be applied on the other side without
-- any unnecessary rebasing.
performSync' :: String -> ConnectedMonadStack ()
performSync' selfHost = do
    putLine "sync"
    putLine selfHost
    commits <- lift S.getCommits
    toTransmit <- performSync'' commits []
    performSync''' toTransmit
    closeConnection

performSync'' :: [Commit] -> [Commit] -> ConnectedMonadStack [Commit]
performSync'' [] t = do
    putLine "done"
    -- Collect "yes" answer from remote.
    getLine
    return t

performSync'' (c:commits) t = do
    putLine $ C.getHash c
    l <- getLine
    if (l == "no")
        then performSync'' commits (c:t)
        else return t

performSync''' :: [Commit] -> ConnectedMonadStack ()
performSync''' [] = putLine "done"
performSync''' (h:t) = do
    cs <- lift $ C.toString h
    putbLine cs
    performSync''' t

-----

readValueStorage :: GrandMonadStack ()
readValueStorage = do
    { storage <- get
    ; let location = concat [storage, "/data"]
    ; printLocalLog $ "Reading storage from " ++ location
    ; hdl <- iOpen location ReadMode
    ; vs <- ibGetContents hdl
    ; S.read vs
    ; printLocalLog $ "Storage was successfully read"
    } `catchError` reportStorageError
    where
        reportStorageError :: String -> GrandMonadStack ()
        reportStorageError e = do
            printLocalLog $ "Couldn't read storage: " ++ e

-----

saveValueStorage :: GrandMonadStack ()
saveValueStorage = do
    { storage <- get
    ; let location = concat [storage, "/data"]
    ; let tmp = location ++ ".tmp"
    ; hdl <- iOpen tmp WriteMode
    ; vs <- S.show
    ; ibPutStr hdl vs
    ; iClose hdl
    -- Lazy IO hack.  Make sure value is on disc before exiting.
    ; hdl' <- iOpen tmp AppendMode
    ; iClose hdl'
    -- End of lazy IO hack.
    ; liftIO $ rawSystem "mv" [tmp, location]
    ; printLocalLog $ "Saved storage to " ++ location
    } `catchError` reportStorageError
    where
        reportStorageError :: String -> GrandMonadStack ()
        reportStorageError e = do
            printLocalLog $ "Saving storage failed: " ++ e

-----

getLine :: ConnectedMonadStack String
getLine = do
    (hdl, _, _) <- get
    iGetLine hdl

-----
-- This will close the handle after it's done.

getRest :: ConnectedMonadStack ByteString
getRest = do
    (hdl, _, _) <- get
    ibGetContents hdl

-----

putLine :: String -> ConnectedMonadStack ()
putLine s = do
    (hdl, _, _) <- get
    iPutStrLn hdl s
    iFlush hdl

-----

writePart :: String -> ConnectedMonadStack ()
writePart s = do
    (hdl, _, _) <- get
    iPutStr hdl s

-----

putbLine :: ByteString -> ConnectedMonadStack ()
putbLine s = do
    (hdl, _, _) <- get
    ibPutStrLn hdl s
    iFlush hdl

-----

getHost :: ConnectedMonadStack String
getHost = do
    (_, host, port) <- get
    return $ concat [host, ":", show port]

-----

closeConnection :: ConnectedMonadStack ()
closeConnection = do
    (hdl, _, _) <- get
    iClose hdl

-----

printLog :: String -> ConnectedMonadStack ()
printLog msg = do
    timeString <- currentTime
    host <- getHost
    iPutStrLn stderr $ concat [timeString, " -- ", host, " -- ", msg]
    iFlush stderr

-----

printLocalLog :: String -> GrandMonadStack ()
printLocalLog msg = do
    timeString <- currentTime
    iPutStrLn stderr $ timeString ++ " -- " ++ msg
    iFlush stderr

-----

currentTime :: (MonadIO m) => m String
currentTime = do
    t <- liftIO getCurrentTime
    return $ formatTime defaultTimeLocale "%m/%d/%Y %H:%M:%S:%q" t
