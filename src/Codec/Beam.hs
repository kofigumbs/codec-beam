module Codec.Beam
  ( -- * Generate BEAM code
    encode
  , Op, Operand(..), Register(..), Access(..), Literal(..), Label
    -- * Incremental encoding
  , Builder, new, append, toLazyByteString
  ) where

import Control.Monad.State.Strict (runState)
import Data.Map (Map, (!))
import Data.Monoid ((<>))
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Word (Word8, Word32)
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Map as Map

import Codec.Beam.Internal
import Codec.Beam.Genop
import qualified Codec.Beam.Bytes as Bytes


-- | Convenience to create code for a BEAM module all at once
encode
  :: String        -- ^ module name
  -> [Op]          -- ^ instructions
  -> BS.ByteString -- ^ return encoded BEAM
encode name =
  toLazyByteString . append (new name)


-- | Create a fresh 'Builder' for a BEAM module
new
  :: String  -- ^ module name
  -> Builder -- ^ return encoding state
new name =
  Builder
    { _moduleName = Atom name_
    , _currentLabelCount = 0
    , _overallLabelCount = 0
    , _functionCount = 0
    , _atomTable = Map.singleton name_ 1
    , _literalTable = []
    , _lambdaTable = []
    , _exportNextLabel = Nothing
    , _toExport = []
    , _code = mempty
    }

  where
    name_ =
      encodeUtf8 (pack name)


-- | Add instructions to the module being encoded
append
  :: Builder -- ^ encoding state
  -> [Op]    -- ^ instructions
  -> Builder -- ^ return new encoding state
append builder =
  foldl collectOp $ builder
    { _currentLabelCount =
        0
    , _overallLabelCount =
        _overallLabelCount builder + _currentLabelCount builder
    }

  where
    collectOp acc (Op opCode state) =
      let (args, newBuilder) = runState state acc in
      appendCode newBuilder (Builder.word8 opCode)
        |> foldl appendOperand $ args


appendOperand :: Builder -> Operand -> Builder
appendOperand builder operand =
  case operand of
    Lit value ->
      tag (Bytes.internal 0) value

    Int value ->
      tag (Bytes.internal 1) value

    Nil ->
      tag (Bytes.internal 2) 0

    Atom name ->
      tag (Bytes.internal 2) |> withAtom name

    Reg (X value) ->
      tag (Bytes.internal 3) value

    Reg (Y value) ->
      tag (Bytes.internal 4) value

    Label value ->
      tag (Bytes.internal 5) $ value + _overallLabelCount builder

    Ext literal ->
      tag (Bytes.external 12) |> withLiteral literal

  where
    tag encoder =
      appendCode builder . Builder.lazyByteString . BS.pack . encoder

    withAtom name toBuilder =
      case Map.lookup name (_atomTable builder) of
        Just value ->
          toBuilder value

        Nothing ->
          let
            old =
              _atomTable builder

            value =
              Map.size old + 1
          in
            (toBuilder value) { _atomTable = Map.insert name value old }

    withLiteral literal toBuilder =
      let
        new =
          literal : _literalTable builder

        value =
          length new
      in
        (toBuilder value) { _literalTable = new }


appendCode :: Builder -> Builder.Builder -> Builder
appendCode builder bytes =
  builder { _code = _code builder <> bytes }


-- | Turn the module encoding state into final BEAM code
toLazyByteString :: Builder -> BS.ByteString
toLazyByteString
  ( Builder
      _
      current
      overall
      functions
      atomTable
      literalTable
      lambdaTable
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
      <> "ExpT" <> alignSection (exports exportTable atomTable)
      <> "Code" <> alignSection (code bytes (overall + current + 1) functions)


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

        EFloat value ->
          pack8 70 <> packDouble value

        EAtom value ->
          pack8 119 <> pack8 (BS.length value) <> value

        EBinary value ->
          pack8 109 <> pack32 (BS.length value) <> value

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

        EMap pairs ->
          mconcat
            [ pack8 116
            , pack32 (length pairs)
            , mconcat $ fmap (\(x, y) -> singleton x <> singleton y) pairs
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


pack32 :: Integral n => n -> BS.ByteString
pack32 =
  Builder.toLazyByteString . Builder.word32BE . fromIntegral


packDouble :: Double -> BS.ByteString
packDouble =
  Builder.toLazyByteString . Builder.doubleBE


(|>) :: a -> (a -> b) -> b
{-# INLINE (|>) #-}
a |> f =
  f a
