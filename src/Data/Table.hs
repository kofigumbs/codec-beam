module Data.Table (Table, empty, singletonOffset, indexOf, encode, size) where

import qualified Data.Map as Map
import qualified Data.List as List


newtype Table k
  = Table (Offset, Map.Map k Int)


{-| See 'singletonOffset'.
 -}
data Offset
  = Zero
  | One


empty :: Table k
empty =
  Table (Zero, Map.empty)


{-| The atom table seems to follow some different rules.
 -  I'm surprised 'Offset' is needed at all, but without it there are
 -  off-by-one encoding errors.
 -}
singletonOffset :: Ord k => k -> Table k
singletonOffset first =
  Table (One, Map.singleton first 1)


indexOf :: Ord k => k -> Table k -> (Int, Table k)
indexOf key table@(Table (offset, map)) =
  case Map.lookup key map of
    Just value ->
      (value, table)

    Nothing ->
      let
        value =
          Map.size map + offsetValue offset
      in
        (value, Table (offset, Map.insert key value map))


encode :: Monoid m => (k -> m) -> Table k -> m
encode func (Table (_, map)) =
  mconcat
    $ fmap (func . fst)
    $ List.sortOn snd
    $ Map.toList map


size :: Table k -> Int
size (Table (_, map)) =
  Map.size map


offsetValue :: Offset -> Int
offsetValue Zero = 0
offsetValue One  = 1
