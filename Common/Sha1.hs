module Cortex.Common.Sha1
    ( lhash
    , hash
    ) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Digest.Pure.SHA (sha1, showDigest)

import Cortex.Common.ByteString

-- Use `showDigest` instead of `bytestringDigest` to get a hex hash.

hash :: BS.ByteString -> BS.ByteString
hash = BS.pack . showDigest . sha1 . toLazyBS

lhash :: LBS.ByteString -> BS.ByteString
lhash = BS.pack . showDigest . sha1
