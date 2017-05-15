module Codec.Beam
  ( Op(..), Term(..), Register(..)
  , Env, new, encode, summarize
  ) where

import Data.Binary.Put (runPut, putWord32be)
import Data.Bits ((.|.), (.&.))
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Word (Word8, Word32)
import qualified Control.Monad.State as State
import qualified Data.Bits as Bits
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Map as Map


{-| Create structurally correct BEAM code.
 -}


data Op
  = Label Int
  | FuncInfo BS.ByteString Int
  | CallOnly Int Int
  | Return
  | Move Term Register


data Term
  = Lit Int
  | Int Int
  | Atom BS.ByteString
  | Reg Register
  | Lab Int


data Register
  = X Int
  | Y Int



-- Incremental encoding


data Env =
  Env
    { moduleName :: BS.ByteString
    , labelCount :: Word32
    , functionCount :: Word32
    , exportNextLabel :: Maybe (BS.ByteString, Int)
    , atomTable :: Map.Map BS.ByteString Word32
    , toExport :: Map.Map (BS.ByteString, Int) (Maybe Int)
    }


new :: BS.ByteString -> [(BS.ByteString, Int)] -> Env
new name exports =
  Env
    { moduleName = name
    , labelCount = 1
    , functionCount = 0
    , exportNextLabel = Nothing
    , atomTable = Map.singleton name 1
    , toExport = Map.fromList (zip exports (repeat Nothing))
    }


encode :: Env -> [Op] -> (B.Builder, Env)
encode env ops =
  State.runState (mconcat <$> mapM fromOp ops) env


summarize :: Env -> BS.ByteString -> BS.ByteString
summarize env code =
  let
    sections =
         "Atom" <> alignSection (encodeAtoms env)
      <> "LocT" <> alignSection (pack32 0)
      <> "StrT" <> alignSection (pack32 0)
      <> "ImpT" <> alignSection (pack32 0)
      <> "ExpT" <> alignSection (encodeExports env)
      <> "Code" <> alignSection (encodeCode env code)
  in
    "FOR1" <> pack32 (BS.length sections + 4) <> "BEAM" <> sections



-- Build environment and code section


fromOp :: Op -> State.State Env B.Builder
fromOp op =
  case op of
    Label uid ->
      do  State.modify $ \env -> env
            { labelCount =
                labelCount env + 1

            , exportNextLabel =
                Nothing

            , toExport =
                case exportNextLabel env of
                  Just function ->
                    Map.insert function (Just uid) (toExport env)

                  Nothing ->
                    toExport env
            }

          instruction 1 [ Lit uid ]

    FuncInfo functionName arity ->
      do  mod <-
            State.gets moduleName

          State.modify $ \env -> env
            { functionCount =
                functionCount env + 1

            , exportNextLabel =
                if Map.member (functionName, arity) (toExport env) then
                  Just (functionName, arity)

                else
                  Nothing
            }

          instruction 2 [ Atom mod, Atom functionName, Lit arity ]

    -- CallOnly arity label ->
    --   instruction 6 [ Lit arity, Lit label ]

    Return ->
      instruction 19 []

    CallOnly arity label ->
      instruction 6 [ Lit arity, Lab label ]

    Move source destination ->
      instruction 64 [ source, Reg destination ]

  where
    instruction opCode args =
      mconcat . (B.word8 opCode :)
        <$> map (B.lazyByteString . BS.pack)
        <$> mapM fromTerm args


fromTerm :: Term -> State.State Env [Word8]
fromTerm term =
  case term of
    Lit value ->
      return $ compact 0 value

    Int value ->
      return $ compact 1 value

    Atom name ->
      compact 2 <$> getAtom name

    Reg (X id) ->
      return $ compact 3 id

    Reg (Y id) ->
      return $ compact 4 id

    Lab id ->
      return $ compact 5 id


getAtom :: BS.ByteString -> State.State Env Word32
getAtom name =
  do  atoms <-
        State.gets atomTable

      case Map.lookup name atoms of
        Just id ->
          return id

        Nothing ->
          do  let id =
                    fromIntegral (Map.size atoms + 1)

              State.modify $ \env -> env
                { atomTable =
                    Map.insert name id atoms
                }

              return id



-- Use the environment to create other sections


encodeAtoms :: Env -> BS.ByteString
encodeAtoms env =
  pack32 (length list) <> mconcat list

  where
    list =
      map fromTuple
        $ List.sortOn snd
        $ Map.toList (atomTable env)

    fromTuple (name, _) =
      pack8 (BS.length name) <> name


encodeExports :: Env -> BS.ByteString
encodeExports env =
  pack32 (length list) <> mconcat list

  where
    list =
      mapMaybe fromTuple $ Map.toList (toExport env)

    fromTuple ((name, arity), maybeLabel) =
      do  labelId <-
            maybeLabel

          nameId <-
            Map.lookup name (atomTable env)

          return $ pack32 nameId <> pack32 arity <> pack32 labelId


encodeCode :: Env -> BS.ByteString -> BS.ByteString
encodeCode env code =
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
    <> pack32 (labelCount env)
    <> pack32 (functionCount env)
    <> code <> intCodeEnd



-- Byte stuff


compact :: (Bits.Bits n, Integral n) => Word8 -> n -> [Word8]
compact tag n =
  if n < 16 then
    [ Bits.shiftL (fromIntegral n) 4 .|. tag
    ]

  else if n < 2048 then
    let
      mostSignificant =
        Bits.shiftR n 3 .&. 0xE0

      continuation =
        0x8
    in
      [ fromIntegral mostSignificant .|. continuation .|. tag
      , fromIntegral n
      ]

  else
    error "TODO"


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
pack32 n =
  runPut (putWord32be (fromIntegral n :: Word32))
