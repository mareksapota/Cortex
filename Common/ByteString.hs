module Cortex.Common.ByteString
    ( toStrictBS
    , toLazyBS
    ) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS

toStrictBS :: LBS.ByteString -> BS.ByteString
toStrictBS = BS.concat . LBS.toChunks

toLazyBS :: BS.ByteString -> LBS.ByteString
toLazyBS s = LBS.fromChunks [s]
