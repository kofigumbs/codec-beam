module Codec.Beam.Encoding
  ( for
  , Literal(..)
  , Lambda(..)
  ) where


import Data.Binary.Put (runPut, putWord16be, putWord32be)
import Data.Map (Map, (!))
import Data.Monoid ((<>))
import Data.Word (Word8, Word32)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Codec.Compression.Zlib as Zlib


data Literal
  = EInt Int
  | EAtom BS.ByteString
  | ETuple [Literal]
  | EList [Literal]


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
  -> Map BS.ByteString Int
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


atoms :: Map BS.ByteString Int -> BS.ByteString
atoms table =
  pack32 (length list) <> mconcat (map encode list)

  where
    encode (name, _) =
      pack8 (BS.length name) <> name

    list =
      List.sortOn snd (Map.toList table)


code :: Builder.Builder -> Int -> Word32 -> BS.ByteString
code builder labelCount functionCount =
  mconcat
    [ pack32 headerLength
    , pack32 instructionSetId
    , pack32 maxOpCode
    , pack32 (fromIntegral labelCount)
    , pack32 functionCount
    , Builder.toLazyByteString builder
    , pack8 intCodeEnd
    ]

  where
    headerLength =
      16

    instructionSetId =
      0

    maxOpCode =
      158

    intCodeEnd =
      3


lambdas :: [Lambda] -> Map BS.ByteString Int -> BS.ByteString
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
        , pack32 oldUnique
        ]

    oldUnique =
      0


exports :: [Export] -> Map BS.ByteString Int -> BS.ByteString
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
      pack32 (length table) <> pack32 (BS.length packed) <> packed

    packed =
      formatMarker <> packLiterals table

    formatMarker =
      pack8 131


packLiterals :: [Literal] -> BS.ByteString
packLiterals =
  foldr (BS.append . singleton) mempty

  where
    singleton lit =
      case lit of
        EInt value | value < 256 ->
          pack8 97 <> pack8 value

        EInt value ->
          pack8 98 <> pack32 value

        EAtom value ->
          pack8 119 <> pack8 (BS.length value) <> value

        ETuple elements | length elements < 256 ->
          mconcat
            [ pack8 104
            , pack8 (length elements)
            , packLiterals elements
            ]

        ETuple elements ->
          mconcat
            [ pack8 105
            , pack32 (length elements)
            , packLiterals elements
            ]

        EList elements ->
          mconcat
            [ pack8 108
            , pack32 (length elements)
            , packLiterals elements
            , pack8 106
            ]


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


pack16 :: Integral n => n -> BS.ByteString
pack16 =
  runPut . putWord16be . fromIntegral


pack32 :: Integral n => n -> BS.ByteString
pack32 =
  runPut . putWord32be . fromIntegral
