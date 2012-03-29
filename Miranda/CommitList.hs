module Cortex.Miranda.CommitList
    ( CommitList
    , empty
    , insert
    , member
    , toList
    ) where

-----

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (partition)

import Cortex.Miranda.Commit (Commit)
import qualified Cortex.Miranda.Commit as Commit

-----

data CommitList = CommitList [Commit] (Set Commit.Hash)
    deriving (Show, Read)

-----

empty :: CommitList
empty = CommitList [] Set.empty

-----

toList :: CommitList -> [Commit]
toList (CommitList l _) = l

-----
-- This is really slow, it goes through the whole list.

insert :: Commit -> CommitList -> (CommitList, [Commit])
insert commit (CommitList l s) = insert' $ partition (< commit) l
    where
        insert' :: ([Commit], [Commit]) -> (CommitList, [Commit])

        insert' (lt, []) = (rebase (reverse lt) [commit] s, [commit])
        insert' (lt, (c:gte))
            -- Commit already exists.
            | c == commit = (CommitList l s, [])
            -- Commit doesn't exist, insert and rebase.
            | otherwise = (rebase (reverse lt) (commit:c:gte) s, commit:c:gte)

-----

member :: Commit.Hash -> CommitList -> Bool
member hash (CommitList _ s) = Set.member hash s

-----

rebase :: [Commit] -> [Commit] -> Set Commit.Hash -> CommitList
rebase lt [] s = CommitList (reverse lt) s
rebase [] (h:t) s = let h' = Commit.rebase Nothing h in
    rebase [h'] t (replace h h' s)
rebase (p:r) (h:t) s = let h' = Commit.rebase (Just p) h in
    rebase (h':p:r) t (replace h h' s)

replace :: Commit -> Commit -> Set Commit.Hash -> Set Commit.Hash
replace old new s = Set.insert (Commit.getHash new) $
    Set.delete (Commit.getHash old) s
