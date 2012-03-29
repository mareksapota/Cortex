module Adria.Backend.ValueStorage
    ( ValueStorage
    , empty
    , set
    , delete
    , lookup
    , lookupAll
    , lookupAllWhere
    , insert
    , member
    , getCommits
    ) where

-----

import Prelude hiding (lookup)
import Control.Monad.Trans (MonadIO)

import Adria.Backend.ValueTree (ValueTree)
import qualified Adria.Backend.ValueTree as VT
import Adria.Backend.CommitList (CommitList)
import qualified Adria.Backend.CommitList as CL
import Adria.Backend.Commit (Commit)
import qualified Adria.Backend.Commit as Commit

-----

data ValueStorage = ValueStorage ValueTree CommitList
    deriving (Show, Read)

-----

empty :: ValueStorage
empty = ValueStorage VT.empty CL.empty

-----

set :: (MonadIO m) => String -> String -> ValueStorage -> m ValueStorage
set key value (ValueStorage vt cl) = do
    c <- Commit.new key (Commit.Set value)
    let vt' = VT.insert key value vt
    let cl' = fst $ CL.insert c cl
    return $ ValueStorage vt' cl'

-----

delete :: (MonadIO m) => String -> ValueStorage -> m ValueStorage
delete key (ValueStorage vt cl) = do
    c <- Commit.new key Commit.Delete
    let vt' = VT.delete key vt
    let cl' = fst $ CL.insert c cl
    return $ ValueStorage vt' cl'

-----

lookup :: String -> ValueStorage -> Maybe String
lookup key (ValueStorage vt _) = VT.lookup key vt

-----

lookupAll :: String -> ValueStorage -> [(String, String)]
lookupAll key (ValueStorage vt _) = VT.lookupAll key vt

-----

lookupAllWhere :: String -> (String -> String -> Bool) -> ValueStorage ->
    [(String, String)]
lookupAllWhere key f (ValueStorage vt _) = VT.lookupAllWhere key f vt

-----

insert :: Commit -> ValueStorage -> (ValueStorage, [Commit])
insert c (ValueStorage vt cl) = let (cl', r) = CL.insert c cl in
    ( ValueStorage (foldl (flip VT.apply) vt r) cl'
    , r
    )

-----

member :: Commit.Hash -> ValueStorage -> Bool
member hash (ValueStorage _ cl) = CL.member hash cl

-----

getCommits :: ValueStorage -> [Commit]
getCommits (ValueStorage _ cl) = CL.toList cl
