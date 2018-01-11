module ByteStringConversion (fromString, toString) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)

toString :: ByteString -> String
toString = unpack . decodeUtf8

fromString :: String -> ByteString
fromString = encodeUtf8 . pack
