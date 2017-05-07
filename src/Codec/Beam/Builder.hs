module Codec.Beam.Builder
  ( Builder, Code, encode
  , Atom, atom
  , Operation, Operand(..)
  , ret, move
  ) where

import qualified Control.Monad.State as State

import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8)
import qualified Data.Set as Set

import qualified Codec.Beam as Beam


{-| Create semantically correct BEAM code.
 -}


type Builder a
  = State.State Env a


data Env
  = Env
      { _atoms :: Set.Set ByteString
      }


data Atom =
  Atom ByteString


data Operand
  = A Atom
  | X Word8


data Operation
  = Operation Word8 [Operand]


type Code =
  Builder [(ByteString, [Operation])]


encode :: ByteString -> Code -> ByteString
encode name builder =
  let
    Env atomTable =
      State.execState builder (Env Set.empty)
  in
    Beam.encode $ Beam.Module
      (name : Set.toList (Set.delete name atomTable))
      []
      []
      []
      (Beam.Code 0 0 [])



-- VALUES


atom :: ByteString -> Builder Atom
atom name =
  do  State.modify $
        \(Env atomTable) -> Env (Set.insert name atomTable)

      return (Atom name)



-- OPERATIONS


ret :: Operation
ret =
  op0 19


move :: Operand -> Operand -> Operation
move =
  op2 64



-- HELPERS


op0 :: Word8 -> Operation
op0 code =
  Operation code []


op2 :: Word8 -> Operand -> Operand -> Operation
op2 code x1 x2 =
  Operation code [x1, x2]
