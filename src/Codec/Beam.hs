module Codec.Beam
  ( Module(..), Statement(..), encode
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Data.Binary.Put (runPut, putWord32be)
import Data.Word (Word32)


-- AST


data Module
  = Module
      { _atoms :: [ByteString]
      , _code :: [Statement]
      , _strings :: [ByteString]
      , _imports :: [(ByteString, ByteString, Int)]
      , _exports :: [(ByteString, Int)]
      }


data Statement
  = TODO



-- SERIALIZE


encode :: Module -> ByteString
encode beam =
  let
    prefixSize bytes =
      pack32 (BS.length bytes) <> align bytes

    chunk toSection lens =
      prefixSize (toSection (lens beam))

    sections =
         "Atom" <> chunk atoms _atoms
      <> "Code" <> chunk code _code
      <> "StrT" <> chunk strings _strings
      <> "ImpT" <> chunk imports _imports
      <> "ExpT" <> chunk exports _exports
  in
    "FOR1" <> pack32 (BS.length sections) <> "BEAM" <> sections


atoms :: [ByteString] -> ByteString
atoms names =
  pack32 (length names) <> concatM atom names

  where
    atom name =
      pack8 (BS.length name) <> name


code :: [Statement] -> ByteString
code _ =
  ""


strings :: [ByteString] -> ByteString
strings _ =
  ""


imports :: [(ByteString, ByteString, Int)] -> ByteString
imports _ =
  ""

exports :: [(ByteString, Int)] -> ByteString
exports _ =
  ""



-- HELPERS


pack8 :: Integral n => n -> ByteString
pack8 =
  BS.singleton . fromIntegral


pack32 :: Integral n => n -> ByteString
pack32 n =
  runPut (putWord32be (fromIntegral n :: Word32))


align :: ByteString -> ByteString
align bytes =
  bytes <> BS.replicate (BS.length bytes `mod` 4) 0


concatM :: Monoid m => (a -> m) -> [a] -> m
concatM f =
  mconcat . map f
