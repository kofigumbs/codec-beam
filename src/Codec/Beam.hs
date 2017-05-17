module Codec.Beam
  ( encode
  , Op(..), Term(..), Register(..)
  , Builder, new, append, toLazyByteString
  ) where

import Data.Binary.Put (runPut, putWord32be)
import Data.Bits ((.|.), (.&.))
import Data.Map ((!))
import Data.Monoid ((<>))
import Data.Word (Word8, Word32)
import qualified Data.Bits as Bits
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Map as Map


{-| Create structurally correct BEAM code.
 -}


data Op
  = Label Int
  | FuncInfo Bool BS.ByteString Int
  | CallOnly Int Int
  | Return
  | IsNil Int Term
  | Move Term Register


data Term
  = Lit Int
  | Int Int
  | Nil
  | Atom BS.ByteString
  | Reg Register
  | Lab Int


data Register
  = X Int
  | Y Int


encode :: BS.ByteString -> [Op] -> BS.ByteString
encode name =
  toLazyByteString . append (new name)



-- Incremental encoding


data Builder =
  Builder
    { moduleName :: Term
    , labelCount :: Word32
    , functionCount :: Word32
    , atomTable :: Map.Map BS.ByteString Int
    , exportNextLabel :: Maybe (BS.ByteString, Int)
    , toExport :: [(BS.ByteString, Int, Int)]
    , code :: Builder.Builder
    }


new :: BS.ByteString -> Builder
new name =
  Builder
    { moduleName = Atom name
    , labelCount = 1
    , functionCount = 0
    , atomTable = Map.singleton name 1
    , exportNextLabel = Nothing
    , toExport = []
    , code = mempty
    }


toLazyByteString :: Builder -> BS.ByteString
toLazyByteString builder =
  let
    sections =
         "Atom" <> alignSection (encodeAtoms builder)
      <> "LocT" <> alignSection (pack32 0)
      <> "StrT" <> alignSection (pack32 0)
      <> "ImpT" <> alignSection (pack32 0)
      <> "ExpT" <> alignSection (encodeExports builder)
      <> "Code" <> alignSection (encodeCode builder)
  in
    "FOR1" <> pack32 (BS.length sections + 4) <> "BEAM" <> sections


append :: Builder -> [Op] -> Builder
append =
  foldl appendOp


appendOp :: Builder -> Op -> Builder
appendOp builder op =
  case op of
    Label uid ->
      builder
        { labelCount =
            labelCount builder + 1

        , exportNextLabel =
            Nothing

        , toExport =
            case exportNextLabel builder of
              Just ( f, a ) ->
                ( f, a, uid ) : toExport builder
              Nothing ->
                toExport builder
        } |>

      instruction 1 [ Lit uid ]

    FuncInfo shouldExport functionName arity ->
      builder
        { functionCount =
            functionCount builder + 1

        , exportNextLabel =
            if shouldExport then
              Just ( functionName, arity )
            else
              Nothing
        } |>

      instruction 2 [ moduleName builder , Atom functionName , Lit arity ]

    CallOnly arity label ->
      instruction 6 [ Lit arity, Lab label ] builder

    Return ->
      instruction 19 [] builder

    IsNil label term ->
      instruction 55 [ Lab label, term ] builder

    Move source destination ->
      instruction 64 [ source, Reg destination ] builder

  where
    instruction opCode args newBuilder =
      appendCode newBuilder (Builder.word8 opCode)
        |> \b -> foldl appendTerm b args


appendTerm :: Builder -> Term -> Builder
appendTerm builder term =
  case term of
    Lit value ->
      tagged 0 value

    Int value ->
      tagged 1 value

    Nil ->
      tagged 2 0

    Atom name ->
      tagged 2 |> withAtom name

    Reg (X value) ->
      tagged 3 value

    Reg (Y value) ->
      tagged 4 value

    Lab value ->
      tagged 5 value

  where
    tagged tag =
      appendCode builder . Builder.lazyByteString . BS.pack . encodeTagged tag

    withAtom name toBuilder =
      case Map.lookup name (atomTable builder) of
        Just value ->
          toBuilder value

        Nothing ->
          let
            old =
              atomTable builder

            value =
              Map.size old + 1
          in
            (toBuilder value)
              { atomTable = Map.insert name value old
              }


appendCode :: Builder -> Builder.Builder -> Builder
appendCode builder bytes =
  builder { code = code builder <> bytes }



-- Use the environment to create other sections


encodeAtoms :: Builder -> BS.ByteString
encodeAtoms builder =
  pack32 (length list) <> mconcat list

  where
    list =
      map fromTuple
        $ List.sortOn snd
        $ Map.toList (atomTable builder)

    fromTuple (name, _) =
      pack8 (BS.length name) <> name


encodeExports :: Builder -> BS.ByteString
encodeExports builder =
  pack32 (length list) <> mconcat list

  where
    list =
      map fromTuple (toExport builder)

    fromTuple (name, arity, labelId) =
      pack32 (atomTable builder ! name) <> pack32 arity <> pack32 labelId


encodeCode :: Builder -> BS.ByteString
encodeCode builder =
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
    <> pack32 (labelCount builder)
    <> pack32 (functionCount builder)
    <> Builder.toLazyByteString (code builder) <> intCodeEnd



-- Byte helpers


encodeTagged :: Word8 -> Int -> [Word8]
encodeTagged tag n =
  if n < 16 then
    [ Bits.shiftL (fromIntegral n) 4 .|. tag
    ]

  else if n < 2048 then
    [ fromIntegral mostSignificant .|. continuation .|. tag
    , fromIntegral n
    ]

  else
    error "TODO"

  where
    mostSignificant =
      Bits.shiftR n 3 .&. 0xE0

    continuation =
        0x8


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


(|>) :: a -> (a -> b) -> b
(|>) =
  flip ($)
