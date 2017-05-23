module Codec.Beam.Function where

import Data.ByteString.Lazy (ByteString)

import qualified Codec.Beam as Beam


{-| Experimental DSL for building up BEAM functions.
 -}


type State =
  ( Int, [Beam.Op] )


many :: [State -> State] -> [Beam.Op]
many =
  snd . foldr ($) ( 1, [] )


public :: ByteString -> Int -> [Beam.Op] -> State -> State
public name arity body ( counter, ops ) =
  ( counter + 2
  , Beam.Label counter
      : Beam.FuncInfo True name arity
      : Beam.Label (counter + 1)
      : body ++ ops
  )


returning :: Beam.Term -> [Beam.Op]
returning term =
  [ Beam.Move term (Beam.X 0)
  , Beam.Return
  ]
