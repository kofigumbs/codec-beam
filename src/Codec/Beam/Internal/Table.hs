module Codec.Beam.Internal.Table
  ( Table, empty, singleton, index, ensure, encode, size
  ) where

import qualified Data.Map as Map
import qualified Data.List as List


data Table k
  = Table
      { _offset :: Int
      , _map :: Map.Map k Int
      }


empty :: Table k
empty =
  Table 0 Map.empty


{-| The atom table seems to follow some different rules.
 -  I\'m surprised '_offset' is needed at all, but without it there are
 -  off-by-one encoding errors.
 -}
singleton :: Ord k => k -> Int -> Table k
singleton key initialValue =
  Table initialValue (Map.singleton key initialValue)


index :: Ord k => k -> Table k -> (Int, Table k)
index key table@(Table offset map) =
  case Map.lookup key map of
    Just value ->
      (value, table)

    Nothing ->
      let value = Map.size map + offset in
      (value, table { _map = Map.insert key value map })


ensure :: Ord k => k -> Table k -> Table k
ensure key =
  snd . index key


encode :: Monoid m => (k -> m) -> Table k -> m
encode func (Table _ map) =
  mconcat
    $ fmap (func . fst)
    $ List.sortOn snd
    $ Map.toList map


size :: Table k -> Int
size (Table _ map) =
  Map.size map
