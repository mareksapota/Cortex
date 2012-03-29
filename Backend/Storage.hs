{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Adria.Backend.Storage
    ( set
    , silentSet
    , delete
    , silentDelete
    , lookup
    , lookupAll
    , lookupAllWhere
    , insert
    , member
    , getCommits
    , Adria.Backend.Storage.show
    , Adria.Backend.Storage.read
    , runVS
    , getMiddlewareHost
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
import Adria.Backend.Commit
    ( Commit (Commit)
    , Operation (Set, Delete)
    )
import qualified Adria.Backend.Commit as Commit
import qualified Adria.Common.Middleware as M

-----

type MVS m a = (MonadState (MVar ValueStorage, String, Int) m, MonadBase IO m) => m a

-----

runVS :: (Monad m, MonadBase IO m) => String -> Int ->
    StateT (MVar ValueStorage, String, Int) m a -> m ()
runVS host port s = do
    mv <- newMVar VS.empty
    runStateT s (mv, host, port)
    return ()

-----

getVS :: MVS m ValueStorage
getVS = do
    (mv, _, _) <- get
    takeMVar mv

-----

putVS :: ValueStorage -> MVS m ()
putVS vs = do
    (mv, _, _) <- get
    putMVar mv vs

-----

getMiddlewareHost :: MVS m (String, Int)
getMiddlewareHost = do
    (_, host, port) <- get
    return (host, port)

-----
-- Notifies Middleware about an operation.

notify :: (MonadIO m, MonadError String m) => [String] -> String -> MVS m ()
notify topics msg = do
    (host, port) <- getMiddlewareHost
    M.send host port topics msg

-----

set :: (MonadIO m, MonadError String m) => String -> String -> MVS m ()
set key value = do
    silentSet key value
    notify ["storage::announce::set", "storage::set::" ++ key] key

-----
-- `set` without Middleware notification.

silentSet :: (MonadIO m) => String -> String -> MVS m ()
silentSet key value = getVS >>= VS.set key value >>= putVS

-----

delete :: (MonadIO m, MonadError String m) => String -> MVS m ()
delete key = do
    silentDelete key
    notify ["storage::announce::delete", "storage::delete::" ++ key] key

-----
-- `delete` witout Middleware notification.

silentDelete :: (MonadIO m) => String -> MVS m ()
silentDelete key = getVS >>= VS.delete key >>= putVS

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

insert :: (MonadIO m, MonadError String m) => Commit -> MVS m ()
insert c = do
    vs <- getVS
    let (vs', commits) = VS.insert c vs
    putVS $ vs'
    forM_ commits announce

announce :: (MonadIO m, MonadError String m) => Commit -> MVS m ()
announce (Commit key (Set _) _ _) =
    notify ["storage::announce::set", "storage::set::" ++ key] key
announce (Commit key (Delete) _ _) =
    notify ["storage::announce::delete", "storage::delete::" ++ key] key

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
