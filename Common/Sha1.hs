module Cortex.Common.Sha1
    ( hash
    , bhash
    ) where

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Digest.Pure.SHA (sha1, showDigest)

hash :: String -> String
hash = showDigest . sha1 . pack

bhash :: ByteString -> String
bhash = showDigest . sha1
