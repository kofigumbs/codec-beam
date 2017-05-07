module Codec.Beam
  ( Module(..), Code(..), Tagged(..), encode
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Data.Binary.Put (runPut, putWord32be)
import Data.Bits (shiftL, (.&.))
import Data.Word (Word8, Word32)


-- AST


data Module
  = Module
      { _atoms :: [ByteString]
      , _strings :: [ByteString]
      , _imports :: [(ByteString, ByteString, Int)]
      , _exports :: [(ByteString, Int)]
      , _code :: Code
      }


data Code
  = Code
      { _labelCount :: Int
      , _functionCount :: Int
      , _instructions :: [(Int, [Tagged])]
      }


data Tagged
  = A Int
  | X Word8


instructionSetId :: Word32
instructionSetId =
  0


maxOpCode :: Word32
maxOpCode =
  159



-- SERIALIZE


encode :: Module -> ByteString
encode beam =
  let
    chunk bytes =
      pack32 (BS.length bytes) <> align bytes

    sections =
         "Atom" <> chunk (atoms (_atoms beam))
      <> "Code" <> chunk (code (_code beam))
      <> "LocT" <> chunk (pack32 0)
      <> "StrT" <> chunk (pack32 0)
      <> "ImpT" <> chunk (pack32 0)
      <> "ExpT" <> chunk (pack32 0)
  in
    "FOR1" <> pack32 (BS.length sections) <> "BEAM" <> sections


atoms :: [ByteString] -> ByteString
atoms names =
  pack32 (length names) <> concatM atom names

  where
    atom name =
      pack8 (BS.length name) <> name


code :: Code -> ByteString
code (Code labelCount functionCount instructions) =
  let
    header =
      mconcat
        [ pack32 instructionSetId
        , pack32 maxOpCode
        , pack32 labelCount
        , pack32 functionCount
        ]
  in
    pack32 (BS.length header) <> header <> concatM instruction instructions


instruction :: (Int, [Tagged]) -> ByteString
instruction (op, args) =
  pack8 op <> concatM (BS.pack . tagged) args

  where
    tagged (A aid) = encodeNumber 2 aid



-- HELPERS


encodeNumber :: Word8 -> Int -> [Word8]
encodeNumber tag n =
  if n < 16 then
    [ shiftL (fromIntegral n) 1 .&. tag ]

  else
    error "TODO"


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
