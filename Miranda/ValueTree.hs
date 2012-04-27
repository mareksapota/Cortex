{-# LANGUAGE Rank2Types, FlexibleContexts, OverloadedStrings #-}

module Cortex.Miranda.ValueTree
    ( ValueTree
    , empty
    , lookup
    , lookupHash
    , lookupAll
    , lookupAllWhere
    , insert
    , delete
    , apply
    , toList
    ) where

-----

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans (MonadIO)
import Control.Monad.Error (MonadError)
import Control.Monad.State (MonadState)
import Data.Maybe (isNothing, fromJust)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Binary (Binary)
import qualified Data.Binary as B

import Cortex.Miranda.Commit (Commit)
import qualified Cortex.Miranda.Commit as Commit

-----

type SIM m a = (MonadIO m, MonadError String m, MonadState String m) => m a

-----

newtype ValueTree = Node (Map BS.ByteString ValueTree, Maybe Commit)
    deriving (Eq)

instance Binary ValueTree where
    put (Node (m, c)) = B.put (m, c)
    get = do
        (m, c) <- B.get
        return $ Node (m, c)

-----

empty :: ValueTree
empty = Node (Map.empty, Nothing)

-----
-- Get value corresponding to given key.

lookup :: BS.ByteString -> ValueTree -> SIM m (Maybe LBS.ByteString)
lookup key t = lookup' (split key) t

lookup' :: [BS.ByteString] -> ValueTree -> SIM m (Maybe LBS.ByteString)
lookup' [] (Node (_, c))
    | isNothing c = return Nothing
    | otherwise = do
        v <- Commit.getValue $ fromJust c
        return $ Just v
lookup' (k:key) (Node (m, _))
    | Map.member k m = lookup' key (m Map.! k)
    | otherwise = return Nothing

-----
-- Get hash of the value corresponding to given key.

lookupHash :: BS.ByteString -> ValueTree -> SIM m (Maybe BS.ByteString)
lookupHash key t = lookupHash' (split key) t

lookupHash' :: [BS.ByteString] -> ValueTree -> SIM m (Maybe BS.ByteString)
lookupHash' [] (Node (_, c))
    | isNothing c = return Nothing
    | otherwise = do
        h <- Commit.getValueHash $ fromJust c
        return $ Just h
lookupHash' (k:key) (Node (m, _))
    | Map.member k m = lookupHash' key (m Map.! k)
    | otherwise = return Nothing

-----
-- Get all commits corresponding to given key prefix.

lookupAll :: BS.ByteString -> ValueTree -> SIM m [(BS.ByteString, LBS.ByteString)]
lookupAll key t = lookupAllWhere key (\_ _ -> True) t

-----
-- Get all commits corresponding to given key prefix where (partial key, value)
-- pair holds given property.

lookupAllWhere :: BS.ByteString -> (BS.ByteString -> LBS.ByteString -> Bool) ->
    ValueTree -> SIM m [(BS.ByteString, LBS.ByteString)]
lookupAllWhere key f vt = lookupAllWhere' (split key) f vt

lookupAllWhere' :: [BS.ByteString] -> (BS.ByteString -> LBS.ByteString -> Bool) ->
    ValueTree -> SIM m [(BS.ByteString, LBS.ByteString)]
lookupAllWhere' [] f vt = getAll f vt
lookupAllWhere' (k:key) f (Node (m, _))
    | Map.member k m = lookupAllWhere' key f (m Map.! k)
    | otherwise = return []

getAll :: (BS.ByteString -> LBS.ByteString -> Bool) -> ValueTree ->
    SIM m [(BS.ByteString, LBS.ByteString)]
getAll f v = getAll' "" f v

getAll' :: BS.ByteString -> (BS.ByteString -> LBS.ByteString -> Bool) ->
    ValueTree -> SIM m [(BS.ByteString, LBS.ByteString)]
getAll' k f (Node (m, Just c)) = do
    rest <- getAll' k f (Node (m, Nothing))
    v <- Commit.getValue c
    if f k v
        then return $ (k, v):rest
        else return rest
getAll' k f (Node (m, Nothing)) = do
    let extend key = if BS.null k then key else BS.concat [k, "::", key]
    l <- mapM
        (\(k', vt') -> getAll' (extend k') f vt')
        (Map.toList m)
    return $ concat l

-----
-- Get all commits from the tree.

toList :: ValueTree -> [Commit]
toList (Node (m, v)) = do
    let l = map toList (snd $ unzip $ Map.toList m)
    if isNothing v
        then concat l
        else concat $ [(fromJust v)]:l

-----

insert :: BS.ByteString -> Commit -> ValueTree -> ValueTree
insert key commit vt = insert' (split key) commit vt

insert' :: [BS.ByteString] -> Commit -> ValueTree -> ValueTree
insert' [] commit (Node (m, _)) = Node (m, Just commit)
insert' (k:key) commit (Node (m, v)) = Node (Map.alter f k m, v)
    where
        f Nothing = Just $ insert' key commit empty
        f (Just n) = Just $ insert' key commit n

-----

delete :: BS.ByteString -> ValueTree -> ValueTree
delete key vt = maybe empty id (delete' (split key) vt)

delete' :: [BS.ByteString] -> ValueTree -> Maybe ValueTree
delete' [] (Node (m, _))
    | Map.null m = Nothing
    | otherwise = Just $ Node (m, Nothing)
delete' (k:key) (Node (m, v))
    | Map.member k m = do
        let m' = Map.update (delete' key) k m
        if Map.null m' && isNothing v
            then Nothing
            else Just $ Node (m', v)
    | otherwise = Just $ Node (m, v)

-----

apply :: Commit -> ValueTree -> ValueTree
apply commit vt
    | Commit.isDelete commit = delete (Commit.getKey commit) vt
    | otherwise = insert (Commit.getKey commit) commit vt

-----

-- Split key at '::' and return list of key parts.
split :: BS.ByteString -> [BS.ByteString]
split "" = []
split key = do
    let (h, t) = BS.foldl f ([], []) key
    reverse ((BS.reverse $ BS.pack h):t)
    where
        f :: (String, [BS.ByteString]) -> Char -> (String, [BS.ByteString])
        f (':':h, t) ':' = ([], (BS.reverse $ BS.pack h):t)
        f (h, t) c = (c:h, t)

-----
