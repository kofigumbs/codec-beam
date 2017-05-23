module Codec.Beam.Function
  ( State, compile
  , public, returning
  ) where

import Data.ByteString.Lazy (ByteString)

import qualified Codec.Beam as Beam


{-| Experimental DSL for building up BEAM functions.
 -}


type State =
  ( [Beam.Op], Int )


compile :: [State -> State] -> [Beam.Op]
compile =
  fst . foldr ($) ( [], 1 )


public :: ByteString -> Int -> [Beam.Op] -> State -> State
public name arity body ( ops, counter ) =
  concatFst ops
    ( Beam.Label counter
        : Beam.FuncInfo True name arity
        : Beam.Label (counter + 1)
        : body
    , counter + 2
    )


returning :: Beam.Term -> [Beam.Op]
returning term =
  [ Beam.Move term (Beam.X 0)
  , Beam.Return
  ]


concatFst :: [a] -> ([a], b) -> ([a], b)
concatFst list ( first, second ) =
  ( first ++ list, second )
