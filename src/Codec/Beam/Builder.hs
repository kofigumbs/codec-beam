module Codec.Beam.Builder
  ( Builder, encode
  , named
  , withAtom
  ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.Set as Set

import qualified Codec.Beam as Beam


{-| Create semantically correct BEAM code.
 -}


data Builder
  = Builder
      { _name :: ByteString
      , _atoms :: Set.Set ByteString
      }


encode :: Builder -> ByteString
encode (Builder name atoms) =
  Beam.encode $ Beam.Module
    (name : Set.toList (Set.delete name atoms))
    []
    []
    []
    []



-- CONSTRUCTING


named :: ByteString -> Builder
named name =
  Builder
    { _name = name
    , _atoms = Set.empty
    }


withAtom :: ByteString -> Builder -> Builder
withAtom atom old =
  old { _atoms = Set.insert atom (_atoms old) }
