module Codec.Beam.Builder
  ( Builder, encode
  , Atom, atom
  ) where

import qualified Control.Monad.State as State

import Data.ByteString.Lazy (ByteString)
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


encode :: ByteString -> Builder a -> ByteString
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
      []



-- CONSTRUCTING


atom :: ByteString -> Builder Atom
atom name =
  do  State.modify $
        \(Env atomTable) -> Env (Set.insert name atomTable)

      return (Atom name)
