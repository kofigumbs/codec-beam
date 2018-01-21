module Codec.Beam
  ( -- * Generate BEAM code
    encode
  , Op, Operand(..), Register(..), Access(..), Literal(..), Label
  ) where

import Control.Monad.State.Strict (runState)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Word (Word8, Word32)
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BS

import Codec.Beam.Internal
import Data.Table (Table)
import qualified Codec.Beam.Internal.Assembler as Asm
import qualified Data.Table as Table


-- | Create code for a BEAM module!
encode
  :: BS.ByteString -- ^ module name
  -> [Op]          -- ^ instructions
  -> BS.ByteString -- ^ return encoded BEAM
encode name =
  toLazyByteString . foldl collectOp (new name)


-- | Create a fresh 'Builder' for a BEAM module
new
  :: BS.ByteString  -- ^ module name
  -> Builder        -- ^ return encoding state
new name =
  Builder
    { _moduleName = Atom name
    , _labelCount = 0
    , _functionCount = 0
    , _atomTable = Table.singleton name 1
    , _literalTable = Table.empty
    , _lambdaTable = []
    , _importTable = Table.empty
    , _exportNextLabel = Nothing
    , _toExport = []
    , _code = mempty
    }


collectOp :: Builder -> Op -> Builder
collectOp acc (Op opCode state) =
  let (args, newBuilder) = runState state acc in
  appendCode newBuilder (Builder.word8 opCode)
    |> foldl appendOperand $ args


appendOperand :: Builder -> Operand -> Builder
appendOperand builder operand =
  case operand of
    Lit value ->
      tag (Asm.internal 0) value

    Int value ->
      tag (Asm.internal 1) value

    Nil ->
      tag (Asm.internal 2) 0

    Atom name ->
      tag (Asm.internal 2) |> withAtom name

    Reg (X value) ->
      tag (Asm.internal 3) value

    Reg (Y value) ->
      tag (Asm.internal 4) value

    Label value ->
      tag (Asm.internal 5) $ value

    Ext literal ->
      tag (Asm.external 4) |> withLiteral literal

  where
    tag encoder =
      appendCode builder . Builder.lazyByteString . BS.pack . encoder

    withAtom name toBuilder =
      Table.index name (_atomTable builder)
        |> \(value, newTable) -> (toBuilder value) { _atomTable = newTable }

    withLiteral literal toBuilder =
      Table.index literal (_literalTable builder)
        |> \(value, newTable) -> (toBuilder value) { _literalTable = newTable }


appendCode :: Builder -> Builder.Builder -> Builder
appendCode builder bytes =
  builder { _code = _code builder <> bytes }


-- | Turn the module encoding state into final BEAM code
toLazyByteString :: Builder -> BS.ByteString
toLazyByteString
  ( Builder
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
        <> Table.encode (withSize pack32 . BS.cons 131 . packLiteral) table


packLiteral :: Literal -> BS.ByteString
packLiteral lit =
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
        , mconcat $ map packLiteral elements
        ]

    ETuple elements ->
      mconcat
        [ pack8 105
        , pack32 (length elements)
        , mconcat $ map packLiteral elements
        ]

    EList elements ->
      mconcat
        [ pack8 108
        , pack32 (length elements)
        , mconcat $ map packLiteral elements
        , pack8 106
        ]

    EMap pairs ->
      mconcat
        [ pack8 116
        , pack32 (length pairs)
        , mconcat $ fmap (\(x, y) -> packLiteral x <> packLiteral y) pairs
        ]


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
