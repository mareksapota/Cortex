{-# LANGUAGE Rank2Types, FlexibleContexts #-}

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
    ) where

-----

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans (MonadIO)
import Control.Monad.Error (MonadError)
import Control.Monad.State (MonadState)
import Data.Maybe (isNothing, fromJust)
import Data.ByteString.Lazy.Char8 (ByteString)

import Cortex.Miranda.Commit (Commit)
import qualified Cortex.Miranda.Commit as Commit

-----

type SIM m a = (MonadIO m, MonadError String m, MonadState String m) => m a

-----

data ValueTree = Node (Map String ValueTree) (Maybe Commit)
    deriving (Eq, Show, Read)

-----

empty :: ValueTree
empty = Node Map.empty Nothing

-----
-- Get value corresponding to given key.

lookup :: String -> ValueTree -> SIM m (Maybe ByteString)
lookup key t = lookup' (split key) t

lookup' :: [String] -> ValueTree -> SIM m (Maybe ByteString)
lookup' [] (Node _ c)
    | isNothing c = return Nothing
    | otherwise = do
        v <- Commit.getValue $ fromJust c
        return $ Just v
lookup' (k:key) (Node m _)
    | Map.member k m = lookup' key (m Map.! k)
    | otherwise = return Nothing

-----
-- Get hash of the value corresponding to given key.

lookupHash :: String -> ValueTree -> SIM m (Maybe String)
lookupHash key t = lookupHash' (split key) t

lookupHash' :: [String] -> ValueTree -> SIM m (Maybe String)
lookupHash' [] (Node _ c)
    | isNothing c = return Nothing
    | otherwise = do
        h <- Commit.getValueHash $ fromJust c
        return $ Just h
lookupHash' (k:key) (Node m _)
    | Map.member k m = lookupHash' key (m Map.! k)
    | otherwise = return Nothing

-----
-- Get all commits corresponding to given key prefix.

lookupAll :: String -> ValueTree -> SIM m [(String, ByteString)]
lookupAll key t = lookupAllWhere key (\_ _ -> True) t

-----
-- Get all commits corresponding to given key prefix where (partial key, value)
-- pair holds given property.

lookupAllWhere :: String -> (String -> ByteString -> Bool) -> ValueTree ->
    SIM m [(String, ByteString)]
lookupAllWhere key f vt = lookupAllWhere' (split key) f vt

lookupAllWhere' :: [String] -> (String -> ByteString -> Bool) -> ValueTree ->
    SIM m [(String, ByteString)]
lookupAllWhere' [] f vt = getAll f vt
lookupAllWhere' (k:key) f (Node m _)
    | Map.member k m = lookupAllWhere' key f (m Map.! k)
    | otherwise = return []

getAll :: (String -> ByteString -> Bool) -> ValueTree ->
    SIM m [(String, ByteString)]
getAll f v = getAll' "" f v

getAll' :: String -> (String -> ByteString -> Bool) -> ValueTree ->
    SIM m [(String, ByteString)]
getAll' k f (Node m (Just c)) = do
    rest <- getAll' k f (Node m Nothing)
    v <- Commit.getValue c
    if f k v
        then return $ (k, v):rest
        else return rest
getAll' k f (Node m Nothing) = do
    l <- mapM
        (\(k', vt') -> getAll' (if null k then k' else k ++ "::" ++ k') f vt')
        (Map.toList m)
    return $ concat l

-----

insert :: String -> Commit -> ValueTree -> ValueTree
insert key commit vt = insert' (split key) commit vt

insert' :: [String] -> Commit -> ValueTree -> ValueTree
insert' [] commit (Node m _) = Node m (Just commit)
insert' (k:key) commit (Node m v)
    | Map.member k m = Node (Map.insert k (insert' key commit (m Map.! k)) m) v
    | otherwise = Node (Map.insert k (insert' key commit empty) m) v

-----

delete :: String -> ValueTree -> ValueTree
delete key vt = delete' (split key) vt

delete' :: [String] -> ValueTree -> ValueTree
delete' key vt = handle $ delete'' key vt
    where
        handle (Just n) = n
        handle Nothing = empty

delete'' :: [String] -> ValueTree -> Maybe ValueTree
delete'' [] (Node m _)
    -- No children, delete node.
    | Map.null m = Nothing
    -- Node has children, just remove the value.
    | otherwise = Just $ Node m Nothing
delete'' (k:key) (Node m v) = checkMap $ Map.update (delete'' key) k m
    where
        checkMap :: Map String ValueTree -> Maybe ValueTree
        checkMap u
            | Map.null u && v == Nothing = Nothing
            | otherwise = Just $ Node u v

-----

apply :: Commit -> ValueTree -> ValueTree
apply commit vt
    | Commit.isDelete commit = delete (Commit.getKey commit) vt
    | otherwise = insert (Commit.getKey commit) commit vt

-----

-- Split key at '::' and return list of key parts.
split :: String -> [String]
split "" = []
split (k:key) = split' k key ""

split' :: Char -> String -> String -> [String]
split' a [] c = [reverse (a:c)]
split' ':' (':':key) c = (reverse c):(split key)
split' a (b:key) c = split' b key (a:c)
