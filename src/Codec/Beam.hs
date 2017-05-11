module Codec.Beam
  ( Builder, encode
  , Instruction, Atom, Label, Tagged(..), Register(..)
  , atom, label, funcInfo, ret, move
  ) where

import qualified Control.Monad.State as State

import Data.Binary.Put (runPut, putWord32be)
import Data.Bits (shiftL, (.&.))
import Data.ByteString.Lazy (ByteString)
import Data.Map ((!))
import Data.Monoid ((<>))
import Data.Word (Word8, Word32)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Map as Map


{-| Create structurally correct BEAM code.
 -}


type Builder a
  = State.State Env a


data Env
  = Env
      { moduleName :: ByteString
      , labelCount :: Word32
      , functionCount :: Word32
      , atomTable :: Map.Map ByteString Int
      }


newtype Instruction
  = Instruction (Word32, [Tagged])


encode
  :: ByteString
  -> [(ByteString, Int, Label)]
  -> Builder [Instruction]
  -> ByteString
encode name exported builder =
  let
    (instructions, env) =
      State.runState builder $ Env
        { moduleName = name
        , labelCount = 0
        , functionCount = 0
        , atomTable = Map.singleton name 1
        }

    sections =
      mconcat
        [ "Atom" <> prefixLength (encodeAtoms env)
        , "Code" <> prefixLength (encodeCode env instructions)
        , "LocT" <> prefixLength (pack32 0)
        , "StrT" <> prefixLength (pack32 0)
        , "ImpT" <> prefixLength (pack32 0)
        , "ExpT" <> prefixLength (pack32 0)
        ]
  in
    "FOR1" <> pack32 (BS.length sections) <> "BEAM" <> sections


encodeAtoms :: Env -> ByteString
encodeAtoms env =
  pack32 (length list) <> concatM fromName list

  where
    fromName name =
      pack8 (BS.length name) <> name

    list =
      map fst
        $ List.sortOn snd
        $ Map.toList (atomTable env)


encodeCode :: Env -> [Instruction] -> ByteString
encodeCode env instructions =
  let
    instructionSetId =
      0

    maxOpCode =
      159

    header =
      mconcat
        [ pack32 instructionSetId
        , pack32 maxOpCode
        , pack32 (labelCount env)
        , pack32 (functionCount env)
        ]

    fromInstruction (Instruction (opCode, args)) =
      pack32 opCode <> BS.pack (concatM (fromTagged env) args)
  in
    prefixLength header <> concatM fromInstruction instructions



-- TERMS


newtype Atom
  = A ByteString
  deriving (Eq, Ord)


atom :: ByteString -> Builder Atom
atom name =
  do  State.modify $
        \env -> env { atomTable = check (atomTable env) }

      return (A name)

  where
    check old =
      if Map.member name old then
        old

      else
        Map.insert name (Map.size old + 1) old


newtype Label
  = L Word32


label :: Builder (Label, Instruction)
label =
  do  next <-
        fmap (+ 1) (State.gets labelCount)

      let id =
            L next

      State.modify $
        \env -> env { labelCount = next }

      return ( id, op 1 [ Label id ] )



-- OPS


data Register
  = X Int
  | Y Int


data Tagged
  = Integer Int
  | Atom Atom
  | Reg Register
  | Label Label


op :: Word32 -> [Tagged] -> Instruction
op code args =
  Instruction (code, args)


funcInfo :: ByteString -> Int -> Builder Instruction
funcInfo name a =
  do  State.modify $
        \env -> env { functionCount = functionCount env + 1 }

      m <- atom =<< State.gets moduleName
      f <- atom name

      return $ op 2 [ Atom m, Atom f, Integer a ]


ret :: Instruction
ret =
  op 19 []


move :: Tagged -> Register -> Instruction
move source destination =
  op 64 [ source, Reg destination ]



-- BYTES


fromTagged :: Env -> Tagged -> [Word8]
fromTagged env t =
  case t of
    Integer value ->
      compact 1 value

    Atom (A name) ->
      compact 2 (atomTable env ! name)

    Reg (X id) ->
      compact 3 id

    Reg (Y id) ->
      compact 4 id

    Label (L id) ->
      compact 5 id


compact :: Integral n => Word8 -> n -> [Word8]
compact tag n =
  if n < 16 then
    [ shiftL (fromIntegral n) 1 .&. tag ]

  else
    error "TODO"


prefixLength :: ByteString -> ByteString
prefixLength bytes =
  pack32 (BS.length bytes) <> align bytes


pack8 :: Integral n => n -> ByteString
pack8 =
  BS.singleton . fromIntegral


pack32 :: Integral n => n -> ByteString
pack32 n =
  runPut (putWord32be (fromIntegral n :: Word32))


align :: ByteString -> ByteString
align bytes =
  bytes <> BS.replicate (BS.length bytes `mod` 4) 0



-- HELPERS


concatM :: Monoid m => (a -> m) -> [a] -> m
concatM f =
  mconcat . map f
