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
import Data.Binary (Binary)
import qualified Data.Binary as B
import Data.Maybe (listToMaybe, fromMaybe)

import Cortex.Miranda.Commit (Commit)
import qualified Cortex.Miranda.Commit as Commit

-----

-- Newest commit is first in the list.
newtype CommitList = CommitList ([Commit], Set Commit.Hash)

instance Binary CommitList where
    put (CommitList (l, s)) = B.put (l, s)
    get = do
        (l, s) <- B.get
        return $ CommitList (l, s)

-----

empty :: CommitList
empty = CommitList ([], Set.empty)

-----
-- Return list of commits, newest first.

toList :: CommitList -> [Commit]
toList (CommitList (l, _)) = l

-----
-- Returns new commit list and list of commits that are greater or equal (mostly
-- this means newer) than the inserted one.  Returned commits are ordered in
-- ascending order (oldest first).

insert :: Commit -> CommitList -> (CommitList, [Commit])
insert commit cl = fromMaybe (cl, []) $ do
    { (cl', r) <- undo commit cl []
    ; cl'' <- redo cl' r
    ; return (cl'', r)
    }

-- Undo commits newer than given commit.
undo :: Commit -> CommitList -> [Commit] -> Maybe (CommitList, [Commit])
undo c (CommitList ([], s)) r = return (CommitList ([], s), c:r)
undo c (CommitList (h:l, s)) r
    | h > c = do
        let s' = Set.delete (Commit.getHash h) s
        undo c (CommitList (l, s')) (h:r)
    | h < c = return (CommitList (h:l, s), c:r)
    | otherwise = Nothing

-- Reapply all commits from the list.
redo :: CommitList -> [Commit] -> Maybe CommitList
redo cl [] = return cl
redo (CommitList (l, s)) (h:t) = do
    let h' = Commit.rebase (listToMaybe l) h
    let s' = Set.insert (Commit.getHash h') s
    redo (CommitList (h':l, s')) t

-----

member :: Commit.Hash -> CommitList -> Bool
member hash (CommitList (_, s)) = Set.member hash s

-----
