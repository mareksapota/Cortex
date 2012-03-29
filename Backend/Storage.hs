{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Adria.Backend.Storage
    ( set
    , delete
    , lookup
    , lookupAll
    , lookupAllWhere
    , insert
    , member
    , getCommits
    , Adria.Backend.Storage.show
    , Adria.Backend.Storage.read
    , runVS
    ) where

-----

import Prelude hiding (lookup)
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Base (MonadBase)
import Control.Concurrent.Lifted
import Data.Maybe (fromJust, isNothing, listToMaybe)

import Adria.Backend.ValueStorage (ValueStorage)
import qualified Adria.Backend.ValueStorage as VS
import Adria.Backend.Commit (Commit)
import qualified Adria.Backend.Commit as Commit

-----

type MVS m a = (MonadState (MVar ValueStorage) m, MonadBase IO m) => m a

-----

runVS :: (Monad m, MonadBase IO m) => StateT (MVar ValueStorage) m a -> m ()
runVS s = do
    mv <- newMVar VS.empty
    runStateT s mv
    return ()

-----

getVS :: MVS m ValueStorage
getVS = get >>= takeMVar

-----

putVS :: ValueStorage -> MVS m ()
putVS vs = get >>= (flip putMVar) vs

-----

set ::  (MonadIO m) => String -> String -> MVS m ()
set key value = getVS >>= VS.set key value >>= putVS

-----

delete :: (MonadIO m) => String -> MVS m ()
delete key = getVS >>= VS.delete key >>= putVS

-----

lookup :: String -> MVS m (Maybe String)
lookup key = do
    vs <- getVS
    let value = VS.lookup key vs
    putVS vs
    return value

-----

lookupAll :: String -> MVS m [(String, String)]
lookupAll key = do
    vs <- getVS
    let values = VS.lookupAll key vs
    putVS vs
    return values

-----

lookupAllWhere :: String -> (String -> String -> Bool) ->
    MVS m [(String, String)]
lookupAllWhere key f = do
    vs <- getVS
    let values = VS.lookupAllWhere key f vs
    putVS vs
    return values

-----

insert :: Commit -> MVS m ()
insert c = do
    vs <- getVS
    putVS $ VS.insert c vs

-----

member :: Commit.Hash -> MVS m Bool
member hash = do
    vs <- getVS
    let b = VS.member hash vs
    putVS vs
    return b

-----

getCommits :: MVS m [Commit]
getCommits = do
    vs <- getVS
    let commits = VS.getCommits vs
    putVS vs
    return commits

-----

show :: MVS m String
show = do
    vs <- getVS
    let s = Prelude.show vs
    putVS vs
    return s

-----

read :: (MonadError String m) => String -> MVS m ()
read s = do
    -- Remove the old MVar value.
    getVS
    let vs = maybeRead s
    when (isNothing vs) (throwError "Couldn't parse ValueStorage")
    putVS $ fromJust vs

maybeRead :: String -> Maybe ValueStorage
maybeRead = fmap fst . listToMaybe . reads
