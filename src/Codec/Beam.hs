module Codec.Beam
  ( encode
  , Op(..), Operand(..), Register(..)
  , Encoding.Literal(..)
  , Builder, new, append, toLazyByteString
  ) where

import Data.Monoid ((<>))
import Data.Word (Word8, Word32)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map

import qualified Codec.Beam.Bytes as Bytes
import qualified Codec.Beam.Encoding as Encoding


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
  | Jump Label
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
  | ExtLiteral Encoding.Literal


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
    , literalTable :: [Encoding.Literal]
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
toLazyByteString
  Builder
    { currentLabelCount = current
    , overallLabelCount = overall
    , functionCount = functions
    , atomTable = atoms
    , literalTable = literals
    , toExport = exports
    , code = code
    } =
  Encoding.for (overall + current + 1) functions atoms literals exports code


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

    Jump label ->
      instruction 61 [ Lab label ] builder

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


(|>) :: a -> (a -> b) -> b
(|>) =
  flip ($)
