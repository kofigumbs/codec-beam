module Codec.Beam
  ( encode
  , Op(..), Operand(..), Register(..), Literal(..)
  , Builder, new, append, toLazyByteString
  ) where

import Data.Binary.Put (runPut, putWord32be)
import Data.Map ((!))
import Data.Monoid ((<>))
import Data.Word (Word8, Word32)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Codec.Compression.Zlib as Zlib

import qualified Codec.Beam.Bytes as Bytes


{-| Create structurally correct BEAM code.
 -}


data Op
  = Label Label
  | FuncInfo BS.ByteString Label
  | Call Int Label
  | CallOnly Int Label
  | Allocate Int Int
  | Deallocate Int
  | Return
  | IsEq Label Operand Operand
  | IsNe Label Operand Operand
  | IsEqExact Label Operand Operand
  | IsNeExact Label Operand Operand
  | IsNil Label Operand
  | Move Operand Register
  | GetList Operand Register Register
  | GetTupleElement Register Int Register
  | SetTupleElement Operand Register Int
  | PutList Operand Operand Register
  | PutTuple Int Register
  | Put Operand
  | CallFun Int


data Operand
  = Lit Int
  | Int Int
  | Nil
  | Atom BS.ByteString
  | Reg Register
  | Lab Int
  | ExtLiteral Literal


data Literal
  = Tuple [Literal]
  | SmInt Int


data Register
  = X Int
  | Y Int


type Label
  = Int


encode :: BS.ByteString -> [Op] -> BS.ByteString
encode name ops =
  toLazyByteString $ append True ops (new name)



-- Incremental encoding


data Builder =
  Builder
    { moduleName :: Operand
    , overallLabelCount :: Int
    , currentLabelCount :: Int
    , functionCount :: Word32
    , atomTable :: Map.Map BS.ByteString Int
    , literalTable :: [Literal]
    , exportNextLabel :: Maybe (BS.ByteString, Int)
    , toExport :: [(BS.ByteString, Int, Int)]
    , code :: Builder.Builder
    }


new :: BS.ByteString -> Builder
new name =
  Builder
    { moduleName = Atom name
    , currentLabelCount = 0
    , overallLabelCount = 0
    , functionCount = 0
    , atomTable = Map.singleton name 1
    , literalTable = []
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
      <> "LitT" <> alignSection (encodeLiterals builder)
      <> "ImpT" <> alignSection (pack32 0)
      <> "ExpT" <> alignSection (encodeExports builder)
      <> "Code" <> alignSection (encodeCode builder)
  in
    "FOR1" <> pack32 (BS.length sections + 4) <> "BEAM" <> sections


append :: Bool -> [Op] -> Builder -> Builder
append shouldExport ops old =
  foldl (appendOp shouldExport) builder ops

  where
    builder =
      old
        { currentLabelCount =
            0
        , overallLabelCount =
            overallLabelCount old + currentLabelCount old
        }


appendOp :: Bool -> Builder -> Op -> Builder
appendOp shouldExport builder op =
  case op of
    Label uid ->
      builder
        { currentLabelCount =
            currentLabelCount builder + 1

        , exportNextLabel =
            Nothing

        , toExport =
            case exportNextLabel builder of
              Just (f, a) ->
                (f, a, uid + overallLabelCount builder) : toExport builder
              Nothing ->
                toExport builder
        } |>

      instruction 1 [ Lit (uid + overallLabelCount builder) ]

    FuncInfo functionName arity ->
      builder
        { functionCount =
            functionCount builder + 1

        , exportNextLabel =
            if shouldExport then
              Just (functionName, arity)
            else
              Nothing
        } |>

      instruction 2 [ moduleName builder, Atom functionName, Lit arity ]

    Call arity label ->
      instruction 4 [ Lit arity, Lab label ] builder

    CallOnly arity label ->
      instruction 6 [ Lit arity, Lab label ] builder

    Allocate stackNeed live ->
      instruction 12 [ Lit stackNeed, Lit live ] builder

    Deallocate n ->
      instruction 18 [ Lit n ] builder

    Return ->
      instruction 19 [] builder

    IsEq label term1 term2 ->
      instruction 41 [ Lab label, term1, term2 ] builder

    IsNe label term1 term2 ->
      instruction 42 [ Lab label, term1, term2 ] builder

    IsEqExact label term1 term2 ->
      instruction 43 [ Lab label, term1, term2 ] builder

    IsNeExact label term1 term2 ->
      instruction 44 [ Lab label, term1, term2 ] builder

    IsNil label term ->
      instruction 55 [ Lab label, term ] builder

    Move source destination ->
      instruction 64 [ source, Reg destination ] builder

    GetList source first rest ->
      instruction 65 [ source, Reg first, Reg rest ] builder

    GetTupleElement source element destination ->
      instruction 66 [ Reg source, Lit element, Reg destination ] builder

    SetTupleElement element tuple position ->
      instruction 67 [ element, Reg tuple, Lit position ] builder

    PutList car cdr destination ->
      instruction 69 [ car, cdr, Reg destination ] builder

    PutTuple size destination ->
      instruction 70 [ Lit size, Reg destination ] builder

    Put value ->
      instruction 71 [ value ] builder

    CallFun arity ->
      instruction 75 [ Lit arity ] builder

  where
    instruction opCode args newBuilder =
      appendCode newBuilder (Builder.word8 opCode)
        |> \b -> foldl appendOperand b args


appendOperand :: Builder -> Operand -> Builder
appendOperand builder operand =
  case operand of
    Lit value ->
      tag Bytes.internal 0 value

    Int value ->
      tag Bytes.internal 1 value

    Nil ->
      tag Bytes.internal 2 0

    Atom name ->
      tag Bytes.internal 2 |> withAtom name

    Reg (X value) ->
      tag Bytes.internal 3 value

    Reg (Y value) ->
      tag Bytes.internal 4 value

    Lab value ->
      tag Bytes.internal 5 (value + overallLabelCount builder)

    ExtLiteral literal ->
      tag Bytes.external 12 |> withLiteral literal


  where
    tag encoder value =
      appendCode builder . Builder.lazyByteString . BS.pack . encoder value

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

    withLiteral literal toBuilder =
      let
        new =
          literal : literalTable builder

        value =
          length new
      in
        (toBuilder value)
          { literalTable = new
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


encodeLiterals :: Builder -> BS.ByteString
encodeLiterals builder =
  pack32 (BS.length encoded) <> Zlib.compress encoded

  where
    encoded =
      pack32 (length literals) <> values

    values =
      foldr (BS.append . appendLiteral) "" literals

    literals =
      literalTable builder


appendLiteral :: Literal -> BS.ByteString
appendLiteral literal =
  case literal of
    Tuple contents ->
      let
        magicTag =
          pack8 131

        smTupleTag =
          pack8 104

        arity =
          pack8 (length contents)

        elements =
          foldr (BS.append . appendLiteral) "" contents

        encoded =
          magicTag <> smTupleTag <> arity <> elements
      in
        pack32 (BS.length encoded) <> encoded

    SmInt value ->
      let
        smIntTag =
          pack8 97

        encoded =
          smIntTag <> pack8 value
      in
        encoded



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

    labelCount =
      overallLabelCount builder + currentLabelCount builder + 1
  in
       pack32 headerLength
    <> pack32 instructionSetId
    <> pack32 maxOpCode
    <> pack32 (fromIntegral labelCount)
    <> pack32 (functionCount builder)
    <> Builder.toLazyByteString (code builder) <> intCodeEnd



-- Encoding helpers


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
