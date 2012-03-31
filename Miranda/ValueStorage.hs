{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Cortex.Miranda.ValueStorage
    ( ValueStorage
    , empty
    , set
    , delete
    , lookup
    , lookupHash
    , lookupAll
    , lookupAllWhere
    , insert
    , member
    , getCommits
    ) where

-----

import Prelude hiding (lookup)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Error (MonadError)
import Control.Monad.State (MonadState)

import Cortex.Miranda.ValueTree (ValueTree)
import qualified Cortex.Miranda.ValueTree as VT
import Cortex.Miranda.CommitList (CommitList)
import qualified Cortex.Miranda.CommitList as CL
import Cortex.Miranda.Commit (Commit)
import qualified Cortex.Miranda.Commit as Commit

-----

type SIM m a = (MonadIO m, MonadError String m, MonadState String m) => m a

-----

data ValueStorage = ValueStorage ValueTree CommitList
    deriving (Show, Read)

-----

empty :: ValueStorage
empty = ValueStorage VT.empty CL.empty

-----

set :: String -> String -> ValueStorage -> SIM m ValueStorage
set key value (ValueStorage vt cl) = do
    c <- Commit.set key value
    let vt' = VT.insert key c vt
    let cl' = fst $ CL.insert c cl
    return $ ValueStorage vt' cl'

-----

delete :: (MonadIO m) => String -> ValueStorage -> m ValueStorage
delete key (ValueStorage vt cl) = do
    c <- Commit.delete key
    let vt' = VT.delete key vt
    let cl' = fst $ CL.insert c cl
    return $ ValueStorage vt' cl'

-----

lookup :: String -> ValueStorage -> SIM m (Maybe String)
lookup key (ValueStorage vt _) = VT.lookup key vt

-----

lookupHash :: String -> ValueStorage -> SIM m (Maybe String)
lookupHash key (ValueStorage vt _) = VT.lookupHash key vt

-----

lookupAll :: String -> ValueStorage -> SIM m [(String, String)]
lookupAll key (ValueStorage vt _) = VT.lookupAll key vt

-----

lookupAllWhere :: String -> (String -> String -> Bool) -> ValueStorage ->
    SIM m [(String, String)]
lookupAllWhere key f (ValueStorage vt _) = VT.lookupAllWhere key f vt

-----

insert :: Commit -> ValueStorage -> ValueStorage
insert c (ValueStorage vt cl) = let (cl', r) = CL.insert c cl in
    ValueStorage (foldl (flip VT.apply) vt r) cl'

-----

member :: Commit.Hash -> ValueStorage -> Bool
member hash (ValueStorage _ cl) = CL.member hash cl

-----

getCommits :: ValueStorage -> [Commit]
getCommits (ValueStorage _ cl) = CL.toList cl
