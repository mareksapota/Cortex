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
    , cleanup
    , getSquashTime
    , setSquashTime
    , updateSquashTime
    ) where

-----

import Prelude hiding (lookup)
import Control.Monad.State
import Control.Monad.Error (throwError, catchError)
import Control.Concurrent.Lifted
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS

import Cortex.Miranda.ValueStorage (ValueStorage)
import qualified Cortex.Miranda.ValueStorage as VS
import Cortex.Miranda.Commit (Commit)
import qualified Cortex.Miranda.Commit as C
import Cortex.Miranda.GrandMonadStack
import Cortex.Common.ErrorIO
import Cortex.Common.Time
import Cortex.Common.ByteString

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

putBack :: (ValueStorage -> GrandMonadStack ValueStorage) -> GrandMonadStack ()
putBack action = do
    vs <- getVS
    (action vs >>= putVS) `catchError` (\e -> do
        { putVS vs
        ; throwError e
        })

-----

set ::  LBS.ByteString -> LBS.ByteString -> GrandMonadStack ()
set key' value = do
    let key = toStrictBS key'
    putBack $ VS.set key value

-----

delete :: LBS.ByteString -> GrandMonadStack ()
delete key' = do
    let key = toStrictBS key'
    putBack $ VS.delete key

-----

lookup :: LBS.ByteString -> GrandMonadStack (Maybe LBS.ByteString)
lookup key' = do
    let key = toStrictBS key'
    vs <- readVS
    VS.lookup key vs

-----

lookupHash :: LBS.ByteString -> GrandMonadStack (Maybe BS.ByteString)
lookupHash key' = do
    let key = toStrictBS key'
    vs <- readVS
    VS.lookupHash key vs

-----

lookupAll :: LBS.ByteString -> GrandMonadStack [(BS.ByteString, LBS.ByteString)]
lookupAll key' = do
    let key = toStrictBS key'
    vs <- readVS
    VS.lookupAll key vs

-----

lookupAllWhere :: LBS.ByteString -> (BS.ByteString -> LBS.ByteString -> Bool) ->
    GrandMonadStack [(BS.ByteString, LBS.ByteString)]
lookupAllWhere key' f = do
    let key = toStrictBS key'
    vs <- readVS
    VS.lookupAllWhere key f vs

-----

insert :: Commit -> GrandMonadStack ()
insert c = do
    vs <- getVS
    putVS $ VS.insert c vs

-----

member :: LBS.ByteString -> GrandMonadStack Bool
member hash' = do
    let hash = toStrictBS hash'
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

show :: GrandMonadStack LBS.ByteString
show = do
    vs <- readVS
    iEncode vs

-----

read :: LBS.ByteString -> GrandMonadStack ()
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
    oldTime <- takeMVar mv
    -- Take the higher value.
    putMVar mv $ max time oldTime

-----

updateSquashTime :: GrandMonadStack ()
updateSquashTime = do
    time <- getBigEndianTimestamp
    setSquashTime time

-----

cleanup :: GrandMonadStack ()
cleanup = do
    vs <- readVS
    let c = filter C.isSet $ VS.getCommits vs
    storage <- get
    -- Drop the storage path.
    l <- mapM
        ( (liftM $ reverse . (takeWhile (/= '/')) . reverse)
        . C.getLocation
        ) c
    iRawSystem "Miranda/cleanup.py" (storage:l)

-----
