module Cortex.Miranda.ValueTree
    ( ValueTree
    , empty
    , lookup
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

import Cortex.Miranda.Commit (Commit (Commit))
import qualified Cortex.Miranda.Commit as Commit

-----

data ValueTree = Node (Map String ValueTree) (Maybe String)
    deriving (Show, Read, Eq)

-----

empty :: ValueTree
empty = Node Map.empty Nothing

-----

lookup :: String -> ValueTree -> Maybe String
lookup key t = lookup' (split key) t

lookup' :: [String] -> ValueTree -> Maybe String
lookup' [] (Node _ v) = v
lookup' (k:key) (Node m _)
    | Map.member k m = lookup' key (m Map.! k)
    | otherwise = Nothing

-----

lookupAll :: String -> ValueTree -> [(String, String)]
lookupAll key t = lookupAllWhere key (\_ _ -> True) t

-----

lookupAllWhere :: String -> (String -> String -> Bool) -> ValueTree ->
    [(String, String)]
lookupAllWhere key f t = lookupAllWhere' (split key) f t

lookupAllWhere' :: [String] -> (String -> String -> Bool) -> ValueTree ->
    [(String, String)]
lookupAllWhere' [] f n = getAll f n
lookupAllWhere' (k:key) f (Node m _)
    | Map.member k m = lookupAllWhere' key f (m Map.! k)
    | otherwise = []

getAll :: (String -> String -> Bool) -> ValueTree -> [(String, String)]
getAll f v = getAll' "" f v

getAll' :: String -> (String -> String -> Bool) -> ValueTree ->
    [(String, String)]
getAll' k f (Node m (Just v))
    | f k v = (k, v):(getAll' k f (Node m Nothing))
    | otherwise = getAll' k f (Node m Nothing)
getAll' k f (Node m Nothing) = concatMap
    (\(k', v') -> getAll' (if null k then k' else k ++ "::" ++ k') f v')
    (Map.toList m)

-----

insert :: String -> String -> ValueTree -> ValueTree
insert key value t = insert' (split key) value t

insert' :: [String] -> String -> ValueTree -> ValueTree
insert' [] value (Node m _) = Node m (Just value)
insert' (k:key) value (Node m v)
    | Map.member k m = Node (Map.insert k (insert' key value (m Map.! k)) m) v
    | otherwise = Node (Map.insert k (insert' key value empty) m) v

-----

delete :: String -> ValueTree -> ValueTree
delete key t = delete' (split key) t

delete' :: [String] -> ValueTree -> ValueTree
delete' key t = handle $ delete'' key t
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
apply (Commit key Commit.Delete _ _) vt = delete key vt
apply (Commit key (Commit.Set value) _ _) vt = insert key value vt

-----

split :: String -> [String]
split "" = []
split (k:key) = split' k key ""

split' :: Char -> String -> String -> [String]
split' a [] c = [reverse (a:c)]
split' ':' (':':key) c = (reverse c):(split key)
split' a (b:key) c = split' b key (a:c)
