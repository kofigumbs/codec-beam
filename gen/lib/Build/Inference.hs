module Build.Inference (run) where

import Types

import qualified Data.Map as Map
import qualified Data.Set as Set


run :: [Line] -> [OpCode] -> [Definition]
run lines opCodes =
  map (toDef (foldr inferLine Map.empty lines)) opCodes


type Env =
  Map.Map String [(Set.Set Type)]


inferLine :: Line -> Env -> Env
inferLine line =
  case line of
    SpecificOp name types ->
      insertTypes name (Set.fromList <$> types)

    Transform patterns _body ->
      flip (foldr inferLeft) patterns

    _ ->
      undefined

inferLeft :: Instruction -> Env -> Env
inferLeft instruction =
  case instruction of
    Op name args ->
      insertTypes name (Set.fromList . _arg_type <$> args)

    C ->
      undefined


insertTypes :: String -> [Set.Set Type] -> Env -> Env
insertTypes =
  Map.insertWith (zipWith Set.union)


toDef :: Env -> OpCode -> Definition
toDef env (OpCode _ code name)=
  maybe undefined
    (Definition name code . fmap Set.toList)
    (Map.lookup name env)
