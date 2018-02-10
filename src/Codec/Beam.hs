module Codec.Beam
  ( encode, Metadata, export, insertModuleInfo
    -- * Syntax
  , Op, X(..), Y(..), F(..), Nil(..), Label(..), Literal(..), Lambda(..), Import(..)
    -- * Argument constraints
  , Register, IsRegister(toRegister), Source, IsSource(toSource)
  , RegisterF, IsRegisterF(toRegisterF), SourceF, IsSourceF(toSourceF)
    -- * BIF helpers
  , importBif0, importBif1, importBif2, importBif3, importBif4
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

import Codec.Beam.Internal.Syntax
import Codec.Beam.Internal.Table (Table)
import qualified Codec.Beam.Internal.Table as Table


-- | Create code for a BEAM module.
encode
  :: BS.ByteString -- ^ module name
  -> [Metadata]
  -> [Op]          -- ^ instructions
  -> BS.ByteString
encode name metadata =
  toLazyByteString . foldl encodeOp (setup metadata (initialEnv name))


-- | Extra information regarding the contents of a BEAM module.
newtype Metadata = Metadata (Env -> Env)


-- | Name and arity of a function that should be made public.
export :: BS.ByteString -> Int -> Metadata
export name arity =
  Metadata $ addExport name arity


-- | The Erlang compiler inserts two functions when compiling source files:
--   @module_info/0@ and @module_info/1@.
--   Some pieces of the Erlang toolchain expect this function to exist.
--   For instance, the shell will crash if you try to use TAB (for auto-completion)
--   on a BEAM module without these functions present.
--   These functions have the same implementation, so you can use this 'Metadata'
--   to have this library generate them for you.
insertModuleInfo :: Metadata
insertModuleInfo =
  Metadata $ \env ->
    let
      withExports =
        addExport "module_info" 0 $ addExport "module_info" 1 env
    in
    foldl encodeOp withExports
      [ Op 1  [FromNewLabel (Label 1)]
      , Op 2  [FromFunctionModule "module_info" 0, FromByteString "module_info", FromUntagged 0]
      , Op 1  [FromNewLabel (Label 2)]
      , Op 64 [FromByteString (_moduleName env), FromX (X 0)]
      , Op 78 [FromUntagged 1, FromImport (Import "erlang" "get_module_info" 1)]
      , Op 19 []
      , Op 1  [FromNewLabel (Label 3)]
      , Op 2  [FromFunctionModule "module_info" 1, FromByteString "module_info", FromUntagged 1]
      , Op 1  [FromNewLabel (Label 4)]
      , Op 64 [FromX (X 0), FromX (X 1)]
      , Op 64 [FromByteString (_moduleName env), FromX (X 0)]
      , Op 78 [FromUntagged 2, FromImport (Import "erlang" "get_module_info" 2)]
      , Op 19 []
      ]


addExport :: BS.ByteString -> Int -> Env -> Env
addExport name arity env =
  env { _exporting = Set.insert (name, arity) (_exporting env) }



-- ASSEMBLER
-- https://github.com/erlang/otp/blob/master/lib/compiler/src/beam_asm.erl


data Env =
  Env
    { _moduleName :: BS.ByteString
    , _labelCount :: Word32
    , _functionCount :: Word32
    , _atomTable :: Table BS.ByteString
    , _literalTable :: Table Literal
    , _lambdaTable :: Table Lambda
    , _importTable :: Table Import
    , _exportTable :: Table (BS.ByteString, Int, Int)
    , _exportNextLabel :: Maybe (BS.ByteString, Int)
    , _exporting :: Set.Set (BS.ByteString, Int)
    , _maxOpCode :: Word8
    , _code :: Builder.Builder
    }


initialEnv :: BS.ByteString -> Env
initialEnv name =
  Env
    { _moduleName = name
    , _labelCount = 1
    , _functionCount = 0
    , _atomTable = Table.singleton name 1
    , _literalTable = Table.empty
    , _lambdaTable = Table.empty
    , _importTable = Table.empty
    , _exportTable = Table.empty
    , _exportNextLabel = Nothing
    , _exporting = Set.empty
    , _maxOpCode = 1
    , _code = mempty
    }


setup :: [Metadata] -> Env -> Env
setup list env =
  case list of
    [] ->
      env

    Metadata run : rest ->
      setup rest (run env)


encodeOp :: Env -> Op -> Env
encodeOp env (Op opCode args) =
  foldl encodeArgument (appendCode withMaxOp (Builder.word8 opCode)) args

  where
    withMaxOp =
      env { _maxOpCode = max opCode (_maxOpCode env) }


encodeArgument :: Env -> Argument -> Env
encodeArgument env argument =
  case argument of
    FromUntagged value ->
      appendTag (encodeTag 0 value) env

    FromNewLabel (Label value) ->
      appendTag (encodeTag 0 value) $ env
        { _labelCount = 1 + _labelCount env
        , _exportNextLabel = Nothing
        , _exportTable =
            maybe id
              (\(f, a) -> Table.ensure (f, a, value))
              (_exportNextLabel env)
              (_exportTable env)
        }

    FromImport import_ ->
      let
        (value, newTable) =
          Table.index import_ (_importTable env)
      in
      appendTag (encodeTag 0 value) $ env
        { _importTable = newTable
        , _atomTable =
            Table.ensure (_import_module import_) $
              Table.ensure (_import_function import_) $ _atomTable env
        }

    FromLambda lambda ->
      let
        (value, newTable) =
          Table.index lambda (_lambdaTable env)
      in
      appendTag (encodeTag 0 value) $ env { _lambdaTable = newTable }

    FromInt value ->
      appendTag (encodeTag 1 value) env

    FromNil Nil ->
      appendTag (encodeTag 2 0) env

    FromFunctionModule name arity ->
      appendTag (encodeTag 2 1) $ env
        { _functionCount = 1 + _functionCount env
        , _exportNextLabel =
            if Set.member (name, arity) (_exporting env) then
              Just (name, arity)
            else
              Nothing
        }

    FromByteString name ->
      let
        (value, newTable) =
          Table.index name (_atomTable env)
      in
      appendTag (encodeTag 2 value) $ env { _atomTable = newTable }

    FromX (X value) ->
      appendTag (encodeTag 3 value) env

    FromY (Y value) ->
      appendTag (encodeTag 4 value) env

    FromLabel (Label value) ->
      appendTag (encodeTag 5 value) env

    FromF (F value) ->
      appendTag (encodeExt 2 value) env

    FromLiteral literal ->
      let
        (value, newTable) =
          Table.index literal (_literalTable env)
      in
      appendTag (encodeExt 4 value) $ env { _literalTable = newTable }

    FromList list ->
      foldr (flip encodeArgument) (appendTag (encodeExt 1 (length list)) env) list


appendTag :: [Word8] -> Env -> Env
appendTag words env =
    appendCode env . Builder.lazyByteString $ BS.pack words


appendCode :: Env -> Builder.Builder -> Env
appendCode env bytes =
  env { _code = _code env <> bytes }


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
      maxOpCode
      bytes
  ) =
  "FOR1" <> pack32 (BS.length sections + 4) <> "BEAM" <> sections

  where
    sections =
         "AtU8" <> alignSection (atoms atomTable)
      <> "StrT" <> alignSection (strings)
      <> "LitT" <> alignSection (literals literalTable)
      <> "FunT" <> alignSection (lambdas lambdaTable atomTable)
      <> "ImpT" <> alignSection (imports importTable atomTable)
      <> "ExpT" <> alignSection (exports exportTable atomTable)
      <> "Code" <> alignSection (code bytes labels functions maxOpCode)



-- SECTIONS


atoms :: Table BS.ByteString -> BS.ByteString
atoms table =
  pack32 (Table.size table) <> Table.encode (withSize pack8) table


-- Explicit string literals are unsupported by this library, but mandatory in BEAM.
--
-- Why not support explicit string literals?
--  1. Since Erlang strings are really integer lists,
--     they are easy to add to the literal table.
--     This can be done in "user-land" without library support:
--     @string :: [Int] -> Beam.Literal@
--  2. Since Erlang strings are really integer lists,
--     they are probably a bad idea for your compiler.
--     It seems that most compile-through-Erlang languages prefer bitstrings,
--     which are supported via the 'Binary' literal.
--
--  If your use case requires this table, please reach out.
strings :: BS.ByteString
strings =
  pack32 0


code :: Builder.Builder -> Word32 -> Word32 -> Word8 -> BS.ByteString
code builder labelCount functionCount maxOpCode =
  mconcat
    [ pack32 16  -- header length
    , pack32 0   -- instruction set id
    , pack32 maxOpCode
    , pack32 labelCount
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
        , pack32 0 -- old unique
        ]


imports :: Table Import -> Table BS.ByteString -> BS.ByteString
imports importTable atomTable =
  pack32 (Table.size importTable) <> Table.encode fromImport importTable

  where
    fromImport (Import module_ function arity) =
      mconcat
        [ pack32 (forceIndex module_ atomTable)
        , pack32 (forceIndex function atomTable)
        , pack32 arity
        ]


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



-- COMPACT TERM ENCODING
-- http://beam-wisdoms.clau.se/en/latest/indepth-beam-file.html#beam-term-format


encodeLiteral :: Literal -> BS.ByteString
encodeLiteral lit =
  case lit of
    Atom value ->
      encodeAtom value

    Integer value ->
      encodeInteger value

    Float value ->
      pack8 70 <> packDouble value

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

    ExternalFun (Import module_ function arity) ->
      mconcat
        [ pack8 113
        , encodeAtom module_
        , encodeAtom function
        , encodeInteger arity
        ]


encodeAtom :: BS.ByteString -> BS.ByteString
encodeAtom value =
  pack8 119 <> withSize pack8 value


encodeInteger :: Int -> BS.ByteString
encodeInteger value
  | value < 256 = pack8 97 <> pack8 value
  | otherwise   = pack8 98 <> pack32 value


encodeTag :: Word8 -> Int -> [Word8]
encodeTag tag n
  | n < 0 = manyBytes tag (negative n [])
  | n < 0x10 = oneByte tag n
  | n < 0x800 = twoBytes tag n
  | otherwise = manyBytes tag (positive n [])


encodeExt :: Int -> Int -> [Word8]
encodeExt tag n =
  encodeTag 7 tag ++ encodeTag 0 n


oneByte :: Word8 -> Int -> [Word8]
oneByte tag n =
  [ top4 .|. tag ]

  where
    top4 =
      Bits.shiftL (fromIntegral n) 4


twoBytes :: Word8 -> Int -> [Word8]
twoBytes tag n =
  [ top3 .|. 0x8 {- continuation tag -} .|. tag, bottom8 ]

  where
    top3 =
      fromIntegral $ Bits.shiftR n 3 .&. 0xE0

    bottom8 =
      fromIntegral n


manyBytes :: Word8 -> [Word8] -> [Word8]
manyBytes tag bytes =
  if count <= 8 then
    (packedCount .|. 0x18 {- continuation tag -} .|. tag) : bytes

  else
    (0xF8 {- nested tag -} .|. tag) : encodeTag 0 (count - 9) ++ bytes

  where
    count =
      length bytes

    packedCount =
      fromIntegral $ Bits.shiftL (count - 2) 5


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
