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
import Data.Binary (Binary)
import qualified Data.Binary as B

import Cortex.Miranda.Commit (Commit)
import qualified Cortex.Miranda.Commit as Commit

-----

-- Newest commit is last in the list.
data CommitList = CommitList [Commit] (Set Commit.Hash)

instance Binary CommitList where
    put (CommitList l s) = B.put (l, s)
    get = do
        (l, s) <- B.get
        return $ CommitList l s

-----

empty :: CommitList
empty = CommitList [] Set.empty

-----
-- Return list of commits, newest first.

toList :: CommitList -> [Commit]
toList (CommitList l _) = reverse l

-----
-- Returns new commit list and list of commits that are greater or equal (mostly
-- this means newer) than the inserted one.
--
-- TODO: This is really slow, it goes through the whole list.

insert :: Commit -> CommitList -> (CommitList, [Commit])
insert commit (CommitList l s) = insert' $ partition (< commit) l
    where
        insert' :: ([Commit], [Commit]) -> (CommitList, [Commit])

        -- There are no commits above this one.
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

-- Commits that are already rebased -> commits that should be rebased -> commit
-- hash set that should be updated -> new commit list.
rebase :: [Commit] -> [Commit] -> Set Commit.Hash -> CommitList
-- No commits left, everything is done.
rebase lt [] s = CommitList (reverse lt) s
-- Rebasing first commit.
rebase [] (h:t) s = let h' = Commit.rebase Nothing h in
    rebase [h'] t (replace h h' s)
-- Rebasing further commits.
rebase (p:r) (h:t) s = let h' = Commit.rebase (Just p) h in
    rebase (h':p:r) t (replace h h' s)

-- Remove old hash and insert new hash to the hash set.
replace :: Commit -> Commit -> Set Commit.Hash -> Set Commit.Hash
replace old new s = Set.insert (Commit.getHash new) $
    Set.delete (Commit.getHash old) s

-----
