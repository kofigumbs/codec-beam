module Codec.Beam
  ( -- * Generate BEAM code
    encode
    -- * Syntax
  , Op, X(..), Y(..), F(..), Nil(..), Label(..), Import(..), Literal(..), Lambda(..)
  , Destination, destination, Pair, pair, Field, field
    -- * Argument constraints
  , Register, RegisterF, Source, SourceF
  ) where


import Data.Bits ((.|.), (.&.))
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Word (Word8, Word32)
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.Bits as Bits
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set

import Codec.Beam.Internal.Types
import Data.Table (Table)
import qualified Data.Table as Table


-- | Create code for a BEAM module!
encode
  :: BS.ByteString          -- ^ module name
  -> [(BS.ByteString, Int)] -- ^ functions @(name, arity)@ that should be public
  -> [Op]                   -- ^ instructions
  -> BS.ByteString          -- ^ return encoded BEAM
encode name toExport =
  toLazyByteString . foldl encodeOp (initialEnv name toExport)



-- ASSEMBLER
-- https://github.com/erlang/otp/blob/master/lib/compiler/src/beam_asm.erl


data Env =
  Env
    { _moduleName :: BS.ByteString
    , _labels :: Set.Set Int
    , _functionCount :: Word32
    , _atomTable :: Table BS.ByteString
    , _literalTable :: Table Literal
    , _lambdaTable :: Table Lambda
    , _importTable :: Table Import
    , _exportTable :: Table (BS.ByteString, Int, Int)
    , _exportNextLabel :: Maybe (BS.ByteString, Int)
    , _toExport :: Set.Set (BS.ByteString, Int)
    , _code :: Builder.Builder
    }


initialEnv :: BS.ByteString -> [(BS.ByteString, Int)] -> Env
initialEnv name toExport =
  Env
    { _moduleName = name
    , _labels = mempty
    , _functionCount = 0
    , _atomTable = Table.singleton name 1
    , _literalTable = Table.empty
    , _lambdaTable = Table.empty
    , _importTable = Table.empty
    , _exportTable = Table.empty
    , _exportNextLabel = Nothing
    , _toExport = Set.fromList toExport
    , _code = mempty
    }


encodeOp :: Env -> Op -> Env
encodeOp env (Op opCode args) =
  foldl encodeArgument (appendCode env (Builder.word8 opCode)) args


encodeArgument :: Env -> Argument a -> Env
encodeArgument env argument =
  case argument of
    FromUntagged value ->
      tag (encodeTag 0) value

    FromNewLabel (Label value) ->
      tag (encodeTag 0) value
        |> \env -> env
              { _labels = Set.insert value (_labels env)
              , _exportNextLabel = Nothing
              , _exportTable =
                  maybe id
                    (\(f, a) -> insert (f, a, value))
                    (_exportNextLabel env)
                    (_exportTable env)
              }

    FromImport import_ ->
      tag (encodeTag 0)
        |> \use -> Table.index import_ (_importTable env)
          |> \(value, newTable) -> (use value)
                { _importTable = newTable
                , _atomTable = _atomTable env
                    |> insert (_import_module import_)
                    |> insert (_import_function import_)
                }


    FromLambda lambda ->
      tag (encodeTag 0)
        |> \use -> Table.index lambda (_lambdaTable env)
          |> \(value, newTable) -> (use value) { _lambdaTable = newTable }

    FromInt value ->
      tag (encodeTag 1) value

    FromNil Nil ->
      tag (encodeTag 2) 0

    FromFunctionModule name arity ->
      tag (encodeTag 2) 1
        |> \env -> env
              { _functionCount = 1 + _functionCount env
              , _exportNextLabel =
                  if Set.member (name, arity) (_toExport env) then
                    Just (name, arity)
                  else
                    Nothing
              }

    FromByteString name ->
      tag (encodeTag 2)
        |> \use -> Table.index name (_atomTable env)
          |> \(value, newTable) -> (use value) { _atomTable = newTable }

    FromX (X value) ->
      tag (encodeTag 3) value

    FromY (Y value) ->
      tag (encodeTag 4) value

    FromF (F _value) ->
      undefined

    FromLabel (Label value) ->
      tag (encodeTag 5) value

    FromLiteral literal ->
      tag ((encodeTag 7 4 ++) . encodeTag 0)
        |> \use -> Table.index literal (_literalTable env)
          |> \(value, newTable) -> (use value) { _literalTable = newTable }

    FromDestinations _destinations ->
      undefined

    FromPairs _pairs ->
      undefined

    FromFields _fields ->
      undefined

  where
    tag encoder =
      appendCode env . Builder.lazyByteString . BS.pack . encoder


appendCode :: Env -> Builder.Builder -> Env
appendCode env bytes =
  env { _code = _code env <> bytes }


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
      exportTable
      _
      _
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
      <> "Code" <> alignSection (code bytes (Set.size labels + 1) functions)


atoms :: Table BS.ByteString -> BS.ByteString
atoms table =
  pack32 (Table.size table) <> Table.encode (withSize pack8) table


code :: Builder.Builder -> Int -> Word32 -> BS.ByteString
code builder labelCount functionCount =
  mconcat
    [ pack32 16  -- header length
    , pack32 0   -- instruction set id
    , pack32 158 -- max op code, TODO: extract this
    , pack32 (fromIntegral labelCount)
    , pack32 functionCount
    , Builder.toLazyByteString builder
    , pack8 3    -- int_code_end
    ]


lambdas :: Table Lambda -> Table BS.ByteString -> BS.ByteString
lambdas lambdaTable atomTable =
  pack32 (Table.size lambdaTable) <> Table.encode fromLambda lambdaTable

  where
    fromLambda lambda@(Lambda name arity (Label label) free) =
      mconcat
        [ pack32 (forceIndex name atomTable)
        , pack32 arity
        , pack32 label
        , pack32 (forceIndex lambda lambdaTable)
        , pack32 free
        , pack32 0 -- oldUnique
        ]


imports :: Table Import -> Table BS.ByteString -> BS.ByteString
imports importTable atomTable =
  pack32 (Table.size importTable) <> Table.encode fromImport importTable

  where
    fromImport (Import m f a) =
      pack32 (forceIndex m atomTable) <> pack32 (forceIndex f atomTable) <> pack32 a


exports :: Table (BS.ByteString, Int, Int) -> Table BS.ByteString -> BS.ByteString
exports exportTable atomTable =
  pack32 (Table.size exportTable) <> Table.encode fromTuple exportTable

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
    Integer value | value < 256 ->
      pack8 97 <> pack8 value

    Integer value ->
      pack8 98 <> pack32 value

    Float value ->
      pack8 70 <> packDouble value

    Atom value ->
      pack8 119 <> withSize pack8 value

    Binary value ->
      pack8 109 <> withSize pack32 value

    Tuple elements | length elements < 256 ->
      mconcat
        [ pack8 104
        , pack8 (length elements)
        , mconcat $ map encodeLiteral elements
        ]

    Tuple elements ->
      mconcat
        [ pack8 105
        , pack32 (length elements)
        , mconcat $ map encodeLiteral elements
        ]

    List elements ->
      mconcat
        [ pack8 108
        , pack32 (length elements)
        , mconcat $ map encodeLiteral elements
        , pack8 106
        ]

    Map pairs ->
      mconcat
        [ pack8 116
        , pack32 (length pairs)
        , mconcat $ fmap (\(x, y) -> encodeLiteral x <> encodeLiteral y) pairs
        ]


encodeTag :: Word8 -> Int -> [Word8]
encodeTag tag n
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
    (nested .|. tag) : encodeTag 0 (count - 9) ++ bytes

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


insert :: Ord k => k -> Table k -> Table k
insert k =
  snd . Table.index k


(|>) :: a -> (a -> b) -> b
{-# INLINE (|>) #-}
a |> f =
  f a
