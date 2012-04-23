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
    , squash
    ) where

-----

import Prelude hiding (lookup)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Error (MonadError)
import Control.Monad.State (MonadState)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Binary (Binary)
import qualified Data.Binary as B
import Control.Parallel (par, pseq)

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

instance Binary ValueStorage where
    put (ValueStorage vt cl) = B.put (vt, cl)
    get = do
        (vt, cl) <- B.get
        return $ ValueStorage vt cl

-----

empty :: ValueStorage
empty = ValueStorage VT.empty CL.empty

-----

set :: String -> ByteString -> ValueStorage -> SIM m ValueStorage
set key value (ValueStorage vt cl) = do
    c <- Commit.set key value
    let vt' = VT.insert key c vt
    let cl' = fst $ CL.insert c cl
    vt' `par` cl' `pseq` return $ ValueStorage vt' cl'

-----

delete :: (MonadIO m) => String -> ValueStorage -> m ValueStorage
delete key (ValueStorage vt cl) = do
    c <- Commit.delete key
    let vt' = VT.delete key vt
    let cl' = fst $ CL.insert c cl
    vt' `par` cl' `pseq` return $ ValueStorage vt' cl'

-----

lookup :: String -> ValueStorage -> SIM m (Maybe ByteString)
lookup key (ValueStorage vt _) = VT.lookup key vt

-----

lookupHash :: String -> ValueStorage -> SIM m (Maybe String)
lookupHash key (ValueStorage vt _) = VT.lookupHash key vt

-----

lookupAll :: String -> ValueStorage -> SIM m [(String, ByteString)]
lookupAll key (ValueStorage vt _) = VT.lookupAll key vt

-----

lookupAllWhere :: String -> (String -> ByteString -> Bool) -> ValueStorage ->
    SIM m [(String, ByteString)]
lookupAllWhere key f (ValueStorage vt _) = VT.lookupAllWhere key f vt

-----

insert :: Commit -> ValueStorage -> ValueStorage
insert c (ValueStorage vt cl) = do
    let (cl', r) = CL.insert c cl
    ValueStorage (foldl (flip VT.apply) vt r) cl'

-----

member :: Commit.Hash -> ValueStorage -> Bool
member hash (ValueStorage _ cl) = CL.member hash cl

-----

getCommits :: ValueStorage -> [Commit]
getCommits (ValueStorage _ cl) = CL.toList cl

-----

squash :: ValueStorage -> ValueStorage
squash (ValueStorage vt _) = do
    let l = VT.toList vt
    let cl = foldl (\x c -> fst (CL.insert c x)) CL.empty l
    ValueStorage vt cl

-----
