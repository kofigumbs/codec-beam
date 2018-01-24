module Build.Inference (run) where

import Data.List (findIndices)
import qualified Data.Set as Set

import Types


run :: [Line] -> [OpCode Int] -> [OpCode [[Type]]]
run lines =
  map $ \op@(OpCode _ name arity) ->
    op { _op_args = map Set.toList $ findTypes name arity lines }


findTypes :: String -> Int -> [Line] -> [Set.Set Type]
findTypes target arity lines =
  foldTypes findType arity lines
  where
    findType line =
      case line of
        SpecificOp name args | match name args ->
          map Set.fromList args

        Transform pattern body ->
          let
            findUses (Op name args) | match name args = map findUse args
            findUses _                                = unknown arity

            findUse (Argument (Just name) []) = trackUses target arity name body lines
            findUse (Argument _ types)        = Set.fromList types
          in
          foldTypes findUses arity pattern

        _ ->
          unknown arity

    match name args =
      name == target && length args == arity


trackUses :: String -> Int -> String -> [Instruction] -> [Line] -> Set.Set Type
trackUses target arity name body lines =
  Set.unions $ concatMap trackUse body
  where
    trackUse instruction =
      case instruction of
        Op op args | not (match op args) ->
          map
            (\index -> findTypes op (length args) lines !! index)
            (findIndices ((== Just name) . _arg_name) args)

        _ ->
          []

    match name args =
      name == target && length args == arity


foldTypes :: (a -> [Set.Set Type]) -> Int -> [a] -> [Set.Set Type]
foldTypes f arity =
  foldr (zipWith Set.union . f) (unknown arity)


unknown :: Int -> [Set.Set Type]
unknown arity =
  replicate arity Set.empty
