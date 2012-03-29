module Adria.Middleware.Storage
    ( empty
    , subscribe
    , unsubscribe
    , unsubscribeAll
    , getSubscribers
    , SubscriptionStorage
    , HandleId
    ) where

import Control.Concurrent.Lifted
import Control.Monad.Trans (MonadIO, liftIO)
import System.IO (Handle)
import Network (HostName, PortNumber)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString (ByteString)

-----

-- Handles are not comparable, so they are represented by host and port of the
-- connecting party.
type HandleId = (HostName, PortNumber)
type HandleSet = Map HandleId Handle
-- Map from topic to subscribed clients.
type TopicToHandleMap = Map ByteString HandleSet
-- Map the other way around.
type HandleToTopicMap = Map HandleId (Set ByteString)

data SubscriptionStorage =
    SubscriptionStorage (MVar (TopicToHandleMap, HandleToTopicMap))

-----

empty :: (MonadIO m) => m SubscriptionStorage
empty = do
    v <- liftIO $ newMVar (Map.empty, Map.empty)
    return $ SubscriptionStorage v

-----

subscribe :: (MonadIO m) => ByteString -> HandleId -> Handle ->
    SubscriptionStorage -> m ()
subscribe topic hid hdl (SubscriptionStorage v) = do
    (tth, htt) <- liftIO $ takeMVar v
    let tth' = Map.insertWith Map.union topic (Map.singleton hid hdl) tth
    let htt' = Map.insertWith Set.union hid (Set.singleton topic) htt
    liftIO $ putMVar v (tth', htt')

-----

unsubscribe :: (MonadIO m) =>
    ByteString -> HandleId -> SubscriptionStorage -> m ()
unsubscribe topic hid (SubscriptionStorage v) = do
    (tth, htt) <- liftIO $ takeMVar v
    (tth', htt') <- unsubscribe' topic hid tth htt
    liftIO $ putMVar v (tth', htt')

unsubscribe' :: (MonadIO m) => ByteString -> HandleId -> TopicToHandleMap ->
    HandleToTopicMap -> m (TopicToHandleMap, HandleToTopicMap)
unsubscribe' topic hid tth htt = do
    let tth' = Map.update removeHandle topic tth
    let htt' = Map.update removeTopic hid htt
    return (tth', htt')
    where
        removeTopic :: Set ByteString -> Maybe (Set ByteString)
        removeTopic topics
            | Set.notMember topic topics = Just topics
            -- Only one element - topic.
            | 1 == Set.size topics = Nothing
            | otherwise = Just $ Set.delete topic topics

        removeHandle :: HandleSet -> Maybe HandleSet
        removeHandle handles
            | Map.notMember hid handles = Just handles
            -- Only one lement - hid.
            | 1 == Map.size handles = Nothing
            | otherwise = Just $ Map.delete hid handles

-----

unsubscribeAll :: (MonadIO m) => HandleId -> SubscriptionStorage -> m ()
unsubscribeAll hid (SubscriptionStorage v) = do
    (tth, htt) <- liftIO $ takeMVar v
    let topics = maybe [] Set.toList (Map.lookup hid htt)
    (tth', htt') <- removeTopics topics tth htt
    liftIO $ putMVar v (tth', htt')
    where
        removeTopics :: (MonadIO m) => [ByteString] -> TopicToHandleMap ->
            HandleToTopicMap -> m (TopicToHandleMap, HandleToTopicMap)
        removeTopics [] tth htt = return (tth, htt)
        removeTopics (topic:topics) tth htt = do
            (tth', htt') <- unsubscribe' topic hid tth htt
            removeTopics topics tth' htt'

-----

getSubscribers :: (MonadIO m) => [ByteString] -> SubscriptionStorage ->
    m [(HandleId, Handle)]
getSubscribers topics (SubscriptionStorage v) = do
    (tth, _) <- liftIO $ readMVar v
    let handleSets = map (\t -> maybe Map.empty id (Map.lookup t tth)) topics
    let handleSet = foldl Map.union Map.empty handleSets
    return $ Map.toList handleSet
