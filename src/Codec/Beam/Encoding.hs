module Codec.Beam.Encoding (for, Literal(..), Lambda(..)) where


import Data.Binary.Put (runPut, putWord32be)
import Data.Map ((!))
import Data.Monoid ((<>))
import Data.Word (Word8, Word32)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Codec.Compression.Zlib as Zlib


data Literal
  = Tuple [Literal]
  | SmInt Int


data Lambda
  = Lambda
      { _name :: BS.ByteString
      , _arity :: Int
      , _label :: Int
      , _index :: Int
      , _free :: Int
      }


type Export
  = (BS.ByteString, Int, Int)


for
  :: Int
  -> Word32
  -> Map.Map BS.ByteString Int
  -> [Literal]
  -> [Lambda]
  -> [Export]
  -> Builder.Builder
  -> BS.ByteString
for labelCount functionCount atomTable literalTable lambdaTable exportTable builder =
  let
    sections =
         "Atom" <> alignSection (atoms atomTable)
      <> "LocT" <> alignSection (pack32 0)
      <> "StrT" <> alignSection (pack32 0)
      <> "LitT" <> alignSection (literals literalTable)
      <> "ImpT" <> alignSection (pack32 0)
      <> "FunT" <> alignSection (lambdas lambdaTable atomTable)
      <> "ExpT" <> alignSection (exports exportTable atomTable)
      <> "Code" <> alignSection (code builder labelCount functionCount)
  in
    "FOR1" <> pack32 (BS.length sections + 4) <> "BEAM" <> sections


atoms :: Map.Map BS.ByteString Int -> BS.ByteString
atoms table =
  pack32 (length list) <> mconcat (map encode list)

  where
    encode (name, _) =
      pack8 (BS.length name) <> name

    list =
      List.sortOn snd (Map.toList table)


code :: Builder.Builder -> Int -> Word32 -> BS.ByteString
code builder labelCount functionCount =
  let
    headerLength =
      16

    instructionSetId =
      0

    maxOpCode =
      158

    intCodeEnd =
      pack8 3
  in
       pack32 headerLength
    <> pack32 instructionSetId
    <> pack32 maxOpCode
    <> pack32 (fromIntegral labelCount)
    <> pack32 functionCount
    <> Builder.toLazyByteString builder <> intCodeEnd


lambdas :: [Lambda] -> Map.Map BS.ByteString Int -> BS.ByteString
lambdas lambdaTable atomTable =
  pack32 (length lambdaTable) <> mconcat (map fromLambda lambdaTable)

  where
    fromLambda (Lambda name arity label index free) =
      mconcat
        [ pack32 (atomTable ! name)
        , pack32 arity
        , pack32 label
        , pack32 index
        , pack32 free
        , pack32 0 -- old unique
        ]


exports :: [Export] -> Map.Map BS.ByteString Int -> BS.ByteString
exports exportTable atomTable =
  pack32 (length exportTable) <> mconcat (map fromTuple exportTable)

  where
    fromTuple (name, arity, label) =
      pack32 (atomTable ! name) <> pack32 arity <> pack32 label


literals :: [Literal] -> BS.ByteString
literals table =
  pack32 (BS.length encoded) <> Zlib.compress encoded

  where
    encoded =
      pack32 (length table) <> foldr (BS.append . singleton) "" table


singleton :: Literal -> BS.ByteString
singleton (Tuple contents) = tuple contents
singleton (SmInt value) = smallInt value


tuple :: [Literal] -> BS.ByteString
tuple contents =
  let
    magicTag =
      pack8 131

    smTupleTag =
      pack8 104

    elements =
      foldr (BS.append . singleton) "" contents

    encoded =
      magicTag <> smTupleTag <> pack8 (length contents) <> elements
  in
    pack32 (BS.length encoded) <> encoded


smallInt :: Int -> BS.ByteString
smallInt value =
  let
    tag =
      pack8 97
  in
    tag <> pack8 value


alignSection :: BS.ByteString -> BS.ByteString
alignSection bytes =
  pack32 size <> bytes <> padding

  where
    size =
      BS.length bytes

    padding =
      case mod size 4 of
        0 -> BS.empty
        r -> BS.replicate (4 - r) 0


pack8 :: Integral n => n -> BS.ByteString
pack8 =
  BS.singleton . fromIntegral


pack32 :: Integral n => n -> BS.ByteString
pack32 =
  runPut . putWord32be . fromIntegral
