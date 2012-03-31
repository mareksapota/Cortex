module Cortex.Common.Sha1
    ( hash
    , bhash
    ) where

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Digest.Pure.SHA (sha1)

hash :: String -> String
hash = show . sha1 . pack

bhash :: ByteString -> String
bhash = show . sha1
