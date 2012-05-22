{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings, Rank2Types #-}

module Cortex.Miranda.CommitRaw where

-- This module exports everything for testing purposes.  Outside of tests
-- `Cortex.Miranda.Commit` module should be used instead.

-----

import Control.Monad.Trans (MonadIO)
import Control.Monad.Error (MonadError, throwError)
import Control.Monad.State (MonadState, get)
import System.IO (IOMode (ReadMode))
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Binary (Binary)
import qualified Data.Binary as B
import Data.Maybe (isNothing, fromJust)
import OpenSSL.EVP.Base64 (encodeBase64LBS, decodeBase64LBS)

import qualified Cortex.Common.Sha1 as Sha1
import Cortex.Common.LazyIO
import Cortex.Common.ErrorIO (iEncode, iDecode, iRawSystem, iOpenTempFile)
import Cortex.Common.Time

-----

type Key = BS.ByteString
type Hash = BS.ByteString
type Timestamp = String

data Operation =
      Set Hash
    | Delete
    deriving (Eq)

newtype Commit = Commit (Key, Operation, Hash, Timestamp)

-----

instance Eq Commit where
    (Commit (k1, op1, _, t1)) == (Commit (k2, op2, _, t2)) =
        k1 == k2 && op1 == op2 && t1 == t2

instance Ord Operation where
    compare Delete Delete = EQ
    compare Delete (Set _) = LT
    compare (Set _) Delete = GT
    compare (Set a) (Set b) = compare a b

instance Ord Commit where
    compare (Commit (k1, op1, _, t1)) (Commit (k2, op2, _, t2)) =
        tryOp $ tryKey $ tryTimestamp
            where
                tryTimestamp = compare t1 t2

                tryKey EQ = compare k1 k2
                tryKey o = o

                tryOp EQ = compare op1 op2
                tryOp o = o

-- This only preserves value hash, not the value itself.  To serialize value
-- too, use `toString` and `fromString`.
instance Binary Operation where
    put Delete = B.put (Nothing :: Maybe String)
    put (Set hash) = B.put (Just hash)
    get = do
        op <- B.get
        if (isNothing op)
            then return Delete
            else return $ Set $ fromJust op

instance Binary Commit where
    put (Commit (key, op, hash, ts)) = B.put (key, op, hash, ts)
    get = do
        (key, op, hash, ts) <- B.get
        return $ Commit (key, op, hash, ts)

-----

type SIM m a = (MonadIO m, MonadState String m, MonadError String m) => m a

-----
-- Get location of commit storage.

getLocation :: Commit -> SIM m String
getLocation (Commit (key, Set hash, _, ts)) = do
    storage <- get
    -- Uses the same things as equality testing, the same location can be only
    -- used by identical commits.
    return $ concat
        [ storage
        , "/"
        , BS.unpack key
        , "."
        , BS.unpack hash
        , "."
        , ts
        ]
getLocation (Commit (_, Delete, _, _)) = throwError "Can't open delete commits"

-----
-- Create a new set type commit.

set :: BS.ByteString -> LBS.ByteString -> SIM m Commit
set key value = do
    let hash = Sha1.lhash value
    ts <- getBigEndianTimestamp
    let commit = Commit (key, Set hash, "", ts)
    location <- getLocation commit
    -- Use a temprorary file.  Commits transmitted during sync might already be
    -- present and GHC runtime isn't happy about opening a file for writing,
    -- when somebody might already be reading it.  If someone is reading this
    -- file, then even after `mv` the old file won't disappear, Linux will
    -- remove the old file only after all file descriptors to it are closed.
    (tmp, hdl) <- iOpenTempFile "/tmp" "commit"
    lPutStr hdl value
    lClose hdl
    iRawSystem "mv" [tmp, location]
    return commit

-----
-- Create a new delete type commit.

delete :: (MonadIO m) => BS.ByteString -> m Commit
delete key = do
    ts <- getBigEndianTimestamp
    return $ Commit (key, Delete, "", ts)

-----
-- Read commit value from storage.

getValue :: Commit -> SIM m LBS.ByteString
getValue commit = do
    location <- getLocation commit
    hdl <- lOpenFile location ReadMode
    v <- lGetContents hdl
    lClose hdl
    return v

-----
-- Get Sha1 hash of commit's value.

getValueHash :: (MonadError String m ) => Commit -> m Hash
getValueHash (Commit (_, Delete, _, _)) =
    throwError "Delete commits don't have values"
getValueHash (Commit (_, Set hash, _, _)) = return hash

-----
-- Rebase a commit to a new parent commit.

rebase :: Maybe Commit -> Commit -> Commit
rebase Nothing c = rebase' "" c
rebase (Just (Commit (_, _, parentHash, _))) c = rebase' parentHash c

rebase' :: Hash -> Commit -> Commit
rebase' parentHash (Commit (key, op, _, ts)) = Commit (key, op, newHash, ts)
    where
        newHash :: Hash
        newHash = Sha1.hash $ BS.concat
            [ parentHash
            , " "
            , key
            , " "
            , hashOp op
            , " "
            , BS.pack ts
            ]

        hashOp :: Operation -> BS.ByteString
        hashOp Delete = "Delete"
        hashOp (Set vh) = BS.concat ["Set \"", vh, "\""]

-----

getKey :: Commit -> BS.ByteString
getKey (Commit (key, _, _, _)) = key

-----

getHash :: Commit -> Hash
getHash (Commit (_, _, hash, _)) = hash

-----

isDelete :: Commit -> Bool
isDelete (Commit (_, Delete, _, _)) = True
isDelete _ = False

-----

isSet :: Commit -> Bool
isSet = not . isDelete

-----

toString :: Commit -> SIM m LBS.ByteString
toString c = do
    t <- toString' c
    e <- iEncode t
    return $ encodeBase64LBS e

toString' :: Commit -> SIM m (Key, BS.ByteString, LBS.ByteString, Hash, Hash, Timestamp)
toString' (Commit (key, Delete, hash, ts)) =
    return (key, "delete", "", "", hash, ts)

toString' (Commit (key, Set valueHash, hash, ts)) = do
    value <- getValue $ Commit (key, Set valueHash, hash, ts)
    return (key, "set", value, valueHash, hash, ts)

-----

fromString :: LBS.ByteString -> SIM m Commit
fromString s = do
    let d = decodeBase64LBS s
    (tuple :: (Key, BS.ByteString, LBS.ByteString, Hash, Hash, Timestamp)) <- iDecode d
    fromString' tuple

fromString' :: (Key, BS.ByteString, LBS.ByteString, Hash, Hash, Timestamp) ->
    SIM m Commit

fromString' (key, "delete", _, _, hash, ts) =
    return $ Commit (key, Delete, hash, ts)

fromString' (key, "set", value, valueHash, hash, ts) = do
    let commit = Commit (key, Set valueHash, hash, ts)
    location <- getLocation commit
    (tmp, hdl) <- iOpenTempFile "/tmp" "commit"
    lPutStr hdl value
    lClose hdl
    iRawSystem "mv" [tmp, location]
    return commit

fromString' _ = throwError "Couldn't parse the commit string"

-----
