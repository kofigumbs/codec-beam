module Codec.Beam.Internal.Assembler (encode) where


import Data.Bits ((.|.), (.&.))
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Word (Word8, Word32)
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.Bits as Bits
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BS

import Codec.Beam.Internal
import Data.Table (Table)
import qualified Data.Table as Table


encodeOp :: Env -> Op -> Env
encodeOp acc (Op opCode state) =
  undefined
  -- let (args, newBuilder) = runState state acc in
  -- appendCode newBuilder (Builder.word8 opCode)
  --   |> foldl encodeArgument $ args


encodeArgument :: Env -> Argument -> Env
encodeArgument builder argument =
  case argument of
    Lit value ->
      tag (encode 0) value

    Int value ->
      tag (encode 1) value

    Nil ->
      tag (encode 2) 0

    Atom name ->
      tag (encode 2) |> withAtom name

    Reg (X value) ->
      tag (encode 3) value

    Reg (Y value) ->
      tag (encode 4) value

    Label value ->
      tag (encode 5) $ value

    Ext literal ->
      tag ((encode 7 4 ++) . encode 0) |> withLiteral literal

  where
    tag encoder =
      appendCode builder . Builder.lazyByteString . BS.pack . encoder

    withAtom name toBuilder =
      Table.index name (_atomTable builder)
        |> \(value, newTable) -> (toBuilder value) { _atomTable = newTable }

    withLiteral literal toBuilder =
      Table.index literal (_literalTable builder)
        |> \(value, newTable) -> (toBuilder value) { _literalTable = newTable }


appendCode :: Env -> Builder.Builder -> Env
appendCode builder bytes =
  builder { _code = _code builder <> bytes }


-- | Turn the module encoding state into final BEAM code
toLazyByteString :: Env -> BS.ByteString
toLazyByteString
  ( Env
      _
      labels
      functions
      atomTable
      literalTable
      lambdaTable
      importTable
      _
      exportTable
      bytes
  ) =
  "FOR1" <> pack32 (BS.length sections + 4) <> "BEAM" <> sections

  where
    sections =
         "Atom" <> alignSection (atoms atomTable)
      <> "LocT" <> alignSection (pack32 0)
      <> "StrT" <> alignSection (pack32 0)
      <> "LitT" <> alignSection (literals literalTable)
      <> "ImpT" <> alignSection (pack32 0)
      <> "FunT" <> alignSection (lambdas lambdaTable atomTable)
      <> "ImpT" <> alignSection (imports importTable atomTable)
      <> "ExpT" <> alignSection (exports exportTable atomTable)
      <> "Code" <> alignSection (code bytes (labels + 1) functions)


atoms :: Table BS.ByteString -> BS.ByteString
atoms table =
  pack32 (Table.size table) <> Table.encode (withSize pack8) table


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


lambdas :: [Lambda] -> Table BS.ByteString -> BS.ByteString
lambdas lambdaTable atomTable =
  pack32 (length lambdaTable) <> mconcat (map fromLambda lambdaTable)

  where
    fromLambda (Lambda name arity label index free) =
      mconcat
        [ pack32 (forceIndex name atomTable)
        , pack32 arity
        , pack32 label
        , pack32 index
        , pack32 free
        , pack32 oldUnique
        ]

    oldUnique =
      0


imports :: Table Function -> Table BS.ByteString -> BS.ByteString
imports importTable atomTable =
  pack32 (Table.size importTable) <> Table.encode fromFunction importTable

  where
    fromFunction (Function m f a) =
      pack32 (forceIndex m atomTable) <> pack32 (forceIndex f atomTable) <> pack32 a


exports :: [Export] -> Table BS.ByteString -> BS.ByteString
exports exportTable atomTable =
  pack32 (length exportTable) <> mconcat (map fromTuple exportTable)

  where
    fromTuple (name, arity, label) =
      pack32 (forceIndex name atomTable) <> pack32 arity <> pack32 label


literals :: Table Literal -> BS.ByteString
literals table =
  pack32 (BS.length terms) <> Zlib.compress terms

  where
    terms =
      pack32 (Table.size table)
        <> Table.encode (withSize pack32 . BS.cons 131 . encodeLiteral) table


encodeLiteral :: Literal -> BS.ByteString
encodeLiteral lit =
  case lit of
    EInt value | value < 256 ->
      pack8 97 <> pack8 value

    EInt value ->
      pack8 98 <> pack32 value

    EFloat value ->
      pack8 70 <> packDouble value

    EAtom value ->
      pack8 119 <> withSize pack8 value

    EBinary value ->
      pack8 109 <> withSize pack32 value

    ETuple elements | length elements < 256 ->
      mconcat
        [ pack8 104
        , pack8 (length elements)
        , mconcat $ map encodeLiteral elements
        ]

    ETuple elements ->
      mconcat
        [ pack8 105
        , pack32 (length elements)
        , mconcat $ map encodeLiteral elements
        ]

    EList elements ->
      mconcat
        [ pack8 108
        , pack32 (length elements)
        , mconcat $ map encodeLiteral elements
        , pack8 106
        ]

    EMap pairs ->
      mconcat
        [ pack8 116
        , pack32 (length pairs)
        , mconcat $ fmap (\(x, y) -> encodeLiteral x <> encodeLiteral y) pairs
        ]
encode :: Word8 -> Int -> [Word8]
encode tag n
  | n < 0 = manyBytes tag (negative n [])
  | n < 0x10 = oneByte tag n
  | n < 0x800 = twoBytes tag n
  | otherwise = manyBytes tag (positive n [])


oneByte :: Word8 -> Int -> [Word8]
oneByte tag n =
  [ top4 .|. tag ]

  where
    top4 =
      Bits.shiftL (fromIntegral n) 4


twoBytes :: Word8 -> Int -> [Word8]
twoBytes tag n =
  [ top3 .|. continuation .|. tag, bottom8 ]

  where
    top3 =
      fromIntegral $ Bits.shiftR n 3 .&. 0xE0

    bottom8 =
      fromIntegral n

    continuation =
      0x8


manyBytes :: Word8 -> [Word8] -> [Word8]
manyBytes tag bytes =
  if count <= 8 then
    (packedCount .|. continuation .|. tag) : bytes

  else
    (nested .|. tag) : encode 0 (count - 9) ++ bytes

  where
    count =
      length bytes

    packedCount =
      fromIntegral $ Bits.shiftL (count - 2) 5

    continuation =
      0x18

    nested =
      0xF8


negative :: Int -> [Word8] -> [Word8]
negative n bytes =
  case ( n, bytes ) of
    ( -1, first : _ : _ ) | first > 0x7F ->
      bytes

    _ ->
      withBottom8 negative n bytes


positive :: Int -> [Word8] -> [Word8]
positive n bytes =
  case ( n, bytes ) of
    ( 0, first : _ ) | first < 0x80 ->
      bytes

    _ ->
      withBottom8 positive n bytes


withBottom8 :: (Int -> [Word8] -> a) -> Int -> [Word8] -> a
{-# INLINE withBottom8 #-}
withBottom8 f n bytes =
  f (Bits.shiftR n 8) (fromIntegral n : bytes)


alignSection :: BS.ByteString -> BS.ByteString
alignSection bytes =
  withSize pack32 bytes <> padding

  where
    padding =
      case mod (BS.length bytes) 4 of
        0 -> BS.empty
        r -> BS.replicate (4 - r) 0


withSize :: (Int64 -> BS.ByteString) -> BS.ByteString -> BS.ByteString
withSize f bytes =
  f (BS.length bytes) <> bytes


pack8 :: Integral n => n -> BS.ByteString
pack8 =
  BS.singleton . fromIntegral


pack32 :: Integral n => n -> BS.ByteString
pack32 =
  Builder.toLazyByteString . Builder.word32BE . fromIntegral


packDouble :: Double -> BS.ByteString
packDouble =
  Builder.toLazyByteString . Builder.doubleBE


forceIndex :: Ord k => k -> Table k -> Int
forceIndex k =
  fst . Table.index k


(|>) :: a -> (a -> b) -> b
{-# INLINE (|>) #-}
a |> f =
  f a
