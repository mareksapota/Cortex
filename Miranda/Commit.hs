{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Cortex.Miranda.Commit
    ( Commit
    , Hash
    , set
    , delete
    , getValue
    , getValueHash
    , rebase
    , getKey
    , getHash
    , isDelete
    , isSet
    , toString
    , fromString
    ) where

-----

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Error (MonadError, throwError)
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as S
import System.IO (IOMode (ReadMode, WriteMode), Handle)
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Cortex.Common.Sha1 as Sha1
import Cortex.Common.ErrorIO

-----

type Key = String
type Hash = String
type Timestamp = String

data Operation =
      Set Hash
    | Delete
    deriving (Eq, Show, Read)

data Commit = Commit Key Operation Hash Timestamp
    deriving (Show, Read)

instance Eq Commit where
    (Commit k1 op1 _ t1) == (Commit k2 op2 _ t2) =
        k1 == k2 && op1 == op2 && t1 == t2

instance Ord Operation where
    compare Delete Delete = EQ
    compare Delete (Set _) = LT
    compare (Set _) Delete = GT
    compare (Set a) (Set b) = compare a b

instance Ord Commit where
    compare (Commit k1 op1 _ t1) (Commit k2 op2 _ t2) =
        tryOp $ tryKey $ tryTimestamp
            where
                tryTimestamp = compare t1 t2

                tryKey EQ = compare k1 k2
                tryKey o = o

                tryOp EQ = compare op1 op2
                tryOp o = o

-----
-- Generate current timestamp.

time :: (MonadIO m) => m String
time = do
    t <- liftIO getCurrentTime
    return $ formatTime defaultTimeLocale "%Y.%m.%d %H:%M:%S:%q" t

-----
-- Open commit storage.

open :: (MonadIO m, MonadState String m, MonadError String m) =>
    Commit -> IOMode -> m Handle
open (Commit key (Set hash) _ ts) mode = do
    storage <- S.get
    hdl <- iOpen (concat [storage, "/", key, ".", hash, ".", ts]) mode
    return hdl
open (Commit _ Delete _ _) _ = throwError "Can't open delete commits"

-----
-- Create a new set type commit.

set :: (MonadIO m, MonadState String m, MonadError String m) =>
    String -> ByteString -> m Commit
set key value = do
    let hash = Sha1.bhash value
    ts <- time
    let commit = Commit key (Set hash) "" ts
    hdl <- open commit WriteMode
    ibPutStr hdl value
    iClose hdl
    return commit

-----
-- Create a new delete type commit.

delete :: (MonadIO m) => String -> m Commit
delete key = do
    ts <- time
    return $ Commit key Delete "" ts

-----
-- Read commit value from storage.

getValue :: (MonadIO m, MonadState String m, MonadError String m) =>
    Commit -> m ByteString
getValue commit = do
    hdl <- open commit ReadMode
    value <- ibGetContents hdl
    return value

-----
-- Get Sha1 hash of commit's value.

getValueHash :: (MonadError String m ) => Commit -> m Hash
getValueHash (Commit _ Delete _ _) =
    throwError "Delete commits don't have values"
getValueHash (Commit _ (Set hash) _ _) = return hash

-----
-- Rebase a commit to a new parent commit.

rebase :: Maybe Commit -> Commit -> Commit
rebase Nothing c = rebase' "" c
rebase (Just (Commit _ _ parentHash _)) c = rebase' parentHash c

rebase' :: Hash -> Commit -> Commit
rebase' parentHash (Commit key op _ ts) = Commit key op newHash ts
    where
        newHash :: Hash
        newHash = Sha1.hash $ concat
            [ parentHash
            , " "
            , key
            , " "
            , show op
            , " "
            , ts
            ]

-----

getKey :: Commit -> String
getKey (Commit key _ _ _) = key

-----

getHash :: Commit -> Hash
getHash (Commit _ _ hash _) = hash

-----

isDelete :: Commit -> Bool
isDelete (Commit _ Delete _ _) = True
isDelete _ = False

-----

isSet :: Commit -> Bool
isSet commit = not $ isDelete commit

-----

toString :: (MonadIO m, MonadState String m, MonadError String m) =>
    Commit -> m String

toString (Commit key Delete hash ts) = return $ show $
    (key, Delete, hash, ts)

toString (Commit key op hash ts) = do
    value <- getValue (Commit key op hash ts)
    return $ show $ (key, (Set (BS.unpack value)), hash, ts)

-----

fromString :: (MonadIO m, MonadState String m, MonadError String m) =>
    String -> m Commit

fromString s = do
    let (tuple :: (Key, Operation, Hash, Timestamp)) = read s
    fromString' tuple

fromString' :: (MonadIO m, MonadState String m, MonadError String m) =>
    (Key, Operation, Hash, Timestamp) -> m Commit

fromString' (key, Delete, hash, ts) =
    return $ Commit key Delete hash ts

fromString' (key, (Set value), hash, ts) = do
    let valueHash = Sha1.hash value
    let commit = Commit key (Set valueHash) hash ts
    hdl <- open commit WriteMode
    iPutStr hdl value
    iClose hdl
    return commit
