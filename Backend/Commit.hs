module Adria.Backend.Commit
    ( Commit (Commit)
    , Operation (Set, Delete)
    , Hash
    , rebase
    , new
    , getHash
    ) where

-----

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime)
import System.Locale (defaultTimeLocale)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA (sha1)

-----

type Key = String
type Hash = String
type Timestamp = String

data Operation =
      Set String
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
        tryValue $ tryKey $ tryTimestamp
            where
                tryTimestamp = compare t1 t2

                tryKey EQ = compare k1 k2
                tryKey o = o

                tryValue EQ = compare op1 op2
                tryValue o = o

-----

rebase :: Maybe Commit -> Commit -> Commit
rebase Nothing c = rebase' "" c
rebase (Just (Commit _ _ parentHash _)) c = rebase' parentHash c

rebase' :: String -> Commit -> Commit
rebase' parentHash (Commit key op _ ts) = Commit key op newHash ts
    where
        newHash :: Hash
        newHash = makeHash $ parentHash ++ " " ++ key ++ " " ++
            (show op) ++ " " ++ ts

        makeHash :: String -> Hash
        makeHash = show . sha1 . pack

-----

new :: (MonadIO m) => Key -> Operation -> m Commit
new key op = do
    t <- liftIO getCurrentTime
    let ts = formatTime defaultTimeLocale "%Y.%m.%d %H:%M:%S:%q" t
    return $ Commit key op "" ts

-----

getHash :: Commit -> Hash
getHash (Commit _ _ hash _) = hash
