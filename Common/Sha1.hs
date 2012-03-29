module Cortex.Common.Sha1
    ( hash
    ) where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA (sha1)

hash :: String -> String
hash = show . sha1 . pack
