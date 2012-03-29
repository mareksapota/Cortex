module Adria.Common.GZip
    ( pack
    , unpack
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Codec.Compression.GZip

-- GZip library sucks - this pure code can throw an exception and terminate the
-- program.

-----

pack :: String -> BS.ByteString
pack s = toStrict $ compressWith fastParams $ LBS.pack s

fastParams :: CompressParams
fastParams = defaultCompressParams { compressLevel = bestSpeed }

-----

unpack :: BS.ByteString -> String
unpack s = LBS.unpack $ decompress $ toLazy s

-----

toStrict :: LBS.ByteString -> BS.ByteString
toStrict = BS.concat . LBS.toChunks

toLazy :: BS.ByteString -> LBS.ByteString
toLazy s = LBS.fromChunks [s]
