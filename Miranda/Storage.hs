module Cortex.Miranda.Storage
    ( set
    , delete
    , lookup
    , lookupHash
    , lookupAll
    , lookupAllWhere
    , insert
    , member
    , getCommits
    , runStorage
    , Cortex.Miranda.Storage.show
    , Cortex.Miranda.Storage.read
    , squash
    , getSquashTime
    , setSquashTime
    , updateSquashTime
    ) where

-----

import Prelude hiding (lookup)
import Control.Monad.State
import Control.Monad.Error (throwError, catchError)
import Control.Concurrent.Lifted
import Data.ByteString.Lazy.Char8 (ByteString)

import Cortex.Miranda.ValueStorage (ValueStorage)
import qualified Cortex.Miranda.ValueStorage as VS
import Cortex.Miranda.Commit (Commit)
import qualified Cortex.Miranda.Commit as Commit
import Cortex.Miranda.GrandMonadStack
import Cortex.Common.ErrorIO
import Cortex.Common.Time

-----
-- WARNING
--
-- Each `getVS` call has to be followed by a `putVS` call.  Make sure you use
-- `putVS` even in case on an error, for example by using `putBack`.
-----

-----

runStorage :: GrandMonadStack a -> String -> LesserMonadStack ()
runStorage s location = do
    time <- getBigEndianMinTimestamp
    timeMV <- newMVar time
    vsMV <- newMVar VS.empty
    runStateT (runStateT s location) (timeMV, vsMV)
    return ()

-----

getVS :: GrandMonadStack ValueStorage
getVS = do
    (_, mv) <- lift get
    takeMVar mv

-----

putVS :: ValueStorage -> GrandMonadStack ()
putVS vs = do
    (_, mv) <- lift get
    putMVar mv vs

-----

readVS :: GrandMonadStack ValueStorage
readVS = do
    (_, mv) <- lift get
    readMVar mv

-----
-- Put value storage back in case of an error.

putBack :: ValueStorage -> String -> GrandMonadStack ()
putBack vs e = do
    putVS vs
    throwError e

-----

set ::  String -> ByteString -> GrandMonadStack ()
set key value = do
    vs <- getVS
    (VS.set key value vs >>= putVS) `catchError` (putBack vs)

-----

delete :: String -> GrandMonadStack ()
delete key = do
    vs <- getVS
    (VS.delete key vs >>= putVS) `catchError` (putBack vs)

-----

lookup :: String -> GrandMonadStack (Maybe ByteString)
lookup key = do
    vs <- readVS
    VS.lookup key vs

-----

lookupHash :: String -> GrandMonadStack (Maybe String)
lookupHash key = do
    vs <- readVS
    VS.lookupHash key vs

-----

lookupAll :: String -> GrandMonadStack [(String, ByteString)]
lookupAll key = do
    vs <- readVS
    VS.lookupAll key vs

-----

lookupAllWhere :: String -> (String -> ByteString -> Bool) ->
    GrandMonadStack [(String, ByteString)]
lookupAllWhere key f = do
    vs <- readVS
    VS.lookupAllWhere key f vs

-----

insert :: Commit -> GrandMonadStack ()
insert c = do
    vs <- getVS
    putVS $ VS.insert c vs

-----

member :: Commit.Hash -> GrandMonadStack Bool
member hash = do
    vs <- readVS
    return $ VS.member hash vs

-----

getCommits :: GrandMonadStack [Commit]
getCommits = do
    vs <- readVS
    let commits = VS.getCommits vs
    return commits

-----

squash :: GrandMonadStack ()
squash = do
    vs <- getVS
    putVS $ VS.squash vs

-----

show :: GrandMonadStack ByteString
show = do
    vs <- readVS
    iEncode vs

-----

read :: ByteString -> GrandMonadStack ()
read s = do
    -- This can produce an error, make sure not to use `getVS` before this.
    vs <- iDecode s
    -- Remove the old MVar value.
    getVS
    putVS vs

-----

getSquashTime :: GrandMonadStack String
getSquashTime = do
    (mv, _) <- lift get
    readMVar mv

-----

setSquashTime :: String -> GrandMonadStack ()
setSquashTime time = do
    (mv, _) <- lift get
    swapMVar mv time
    return ()

-----

updateSquashTime :: GrandMonadStack ()
updateSquashTime = do
    time <- getBigEndianTimestamp
    (mv, _) <- lift get
    swapMVar mv time
    return ()

-----
