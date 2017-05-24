module Codec.Beam.Function where

import Data.ByteString.Lazy (ByteString)

import qualified Codec.Beam as Beam


{-| Experimental DSL for building up BEAM functions.
 -}


type Model =
  ( [Beam.Op], Int )


compile :: [Model -> Model] -> [Beam.Op]
compile =
  fst . foldr ($) ( [], 1 )


function :: ByteString -> Int -> [Beam.Op] -> Model -> Model
function name arity body ( ops, counter ) =
  ( Beam.Label counter
      : Beam.FuncInfo name arity
      : Beam.Label (counter + 1)
      : body ++ ops
  , counter + 2
  )


returning :: Beam.Term -> [Beam.Op]
returning term =
  [ Beam.Move term (Beam.X 0)
  , Beam.Return
  ]
