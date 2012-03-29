{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Adria.Middleware.Server (runServer) where

import Control.Concurrent.Lifted (fork)
import Network
import System.IO
    ( stderr
    , BufferMode (LineBuffering)
    , Handle
    )
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Error
import Control.Monad.State
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Adria.Middleware.Storage (SubscriptionStorage, HandleId)
import qualified Adria.Middleware.Storage as Storage
import Adria.Common.ErrorIO

-- Monad stack with IO on the bottom and Error somewhere higher.
type EBI m a = (MonadError String m, MonadIO m, MonadBaseControl IO m) => m a

type ConnectionInfo = (Handle, HandleId, SubscriptionStorage)

-- Monad stack used for connection handling.
type CMS m a = (MonadState ConnectionInfo m) => EBI m a

getHandle :: CMS m Handle
getHandle = do
    (hdl, _, _) <- get
    return hdl

subscribe :: ByteString -> CMS m ()
subscribe topic = do
    (hdl, handleId, storage) <- get
    Storage.subscribe topic handleId hdl storage

unsubscribe :: ByteString -> CMS m ()
unsubscribe topic = do
    (_, handleId, storage) <- get
    Storage.unsubscribe topic handleId storage

-- Unsubscribe this connection from any topics.
unsubscribeAll :: CMS m ()
unsubscribeAll = do
    (_, handleId, storage) <- get
    Storage.unsubscribeAll handleId storage

-- Unsubscribe some other connection from any topics.
removeHandle :: HandleId -> CMS m ()
removeHandle handleId = do
    (_, _, storage) <- get
    Storage.unsubscribeAll handleId storage

getSubscribers :: [ByteString] -> CMS m [(HandleId, Handle)]
getSubscribers topics = do
    (_, _, storage) <- get
    Storage.getSubscribers topics storage

close :: CMS m ()
close = do
    (hdl, _, _) <- get
    iClose hdl
    unsubscribeAll

printLog :: String -> CMS m ()
printLog msg = do
    t <- liftIO getCurrentTime
    let timeString = formatTime defaultTimeLocale "%m/%d/%Y %H:%M:%S:%q" t
    (_, handleId, _) <- get
    let (host, port) = handleId
    let name = host ++ ":" ++ (show port)
    iPutStrLn stderr $ timeString ++ " -- " ++ name ++ " -- " ++ msg
    iFlush stderr

runServer :: Int -> EBI m ()
runServer serverPort = do
    socket <- iListenOn $ PortNumber $ fromIntegral serverPort
    storage <- Storage.empty
    iPutStrLn stderr $ "Server started on port " ++ (show serverPort)
    iFlush stderr
    forever $ catchError (acceptConnection socket storage) reportError
    where
        acceptConnection :: Socket -> SubscriptionStorage -> EBI m ()
        acceptConnection socket storage = do
            (hdl, host, port) <- iAccept socket
            fork $ evalStateT handleConnection (hdl, (host, port), storage)
            -- Ignore result of fork and return proper type.
            return ()

        reportError :: String -> EBI m ()
        reportError e = do
            -- If this operation throws an error, accept loop will exit.
            iPutStrLn stderr $ "Error: " ++ e
            iFlush stderr

-- `fork` discards errors, so `reportError` from `runServer` won't report them,
-- they have to be caught here.
handleConnection :: CMS m ()
handleConnection = catchError handleConnection' reportError
    where
        reportError :: String -> CMS m ()
        reportError e = do
            close
            printLog ("Error: " ++ e)

handleConnection' :: CMS m ()
handleConnection' = do
    hdl <- getHandle
    iSetBuffering hdl LineBuffering
    iGetLine hdl >>= chooseConnectionMode

-----

chooseConnectionMode :: String -> CMS m ()

chooseConnectionMode "subscribe" = do
    printLog "Subscription mode"
    hdl <- getHandle
    forever $ iGetLine hdl >>= changeSubscription
    where
        changeSubscription :: String -> CMS m ()

        changeSubscription s
            | elem ' ' s = throwError $ "Illegal character ' '"
            | elem '|' s = throwError $ "Illegal character '|'"

        changeSubscription ('+':topic) = do
            subscribe $ BS.pack topic
            printLog $ "Subscribed to " ++ topic

        changeSubscription ('-':topic) = do
            unsubscribe $ BS.pack topic
            printLog $ "Unsubscribed from " ++ topic

        changeSubscription _ =
            throwError $ "Expected line starting with '+' or '-'"

chooseConnectionMode "publish" = do
    printLog "Publishing mode"
    topics <- collectTopics
    when (null topics) $ throwError "No topics specified"
    hdl <- getHandle
    msg <- ibGetLine hdl
    sendMessage msg topics
    iClose hdl
    printLog $ "Sent message"
    where
        collectTopics :: CMS m [ByteString]
        collectTopics = collectTopics' []

        collectTopics' :: [ByteString] -> CMS m [ByteString]
        collectTopics' topics = do
            hdl <- getHandle
            topic <- ibGetLine hdl
            if (BS.null topic)
                then return topics
                else collectTopics' (topic:topics)

        sendMessage :: ByteString -> [ByteString] -> CMS m ()
        sendMessage msg topics = do
            handles <- getSubscribers topics
            mapM_ trySend handles
            where
                trySend :: (HandleId, Handle) -> CMS m ()
                trySend (handleId, hdl) =
                    catchError (send hdl) $ \_ -> do
                        -- Something is wrong with this connection, remove it.
                        removeHandle handleId
                        iClose hdl
                        printLog "Error: sending error"

                send :: Handle -> CMS m ()
                send hdl = do
                    ibPutStr hdl (head topics)
                    forM_ (tail topics) $ \t -> do
                        iPutStr hdl "|"
                        ibPutStrLn hdl t
                    iPutStr hdl " "
                    ibPutStrLn hdl msg
                    iFlush hdl

chooseConnectionMode _ = do
    throwError "Unknown connection mode"
