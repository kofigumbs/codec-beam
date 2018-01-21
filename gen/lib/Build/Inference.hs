module Build.Inference (run) where

import Types

import qualified Data.Map as Map
import qualified Data.Set as Set


run :: [Line] -> [OpCode] -> [Definition]
run lines opCodes =
  map (toDef (foldr inferLine mempty lines)) opCodes



-- PRIVATE STUFF


type Env = Map.Map String [Union]
type Union = Set.Set Type


inferLine :: Line -> Env -> Env
inferLine line env =
  case line of
    GenericOp _name ->
      undefined

    SpecificOp name types ->
      trackDef name (fmap Set.fromList types) env

    Transform patterns body ->
      foldr (inferPattern body) env patterns


inferPattern :: [Instruction] -> Instruction -> Env -> Env
inferPattern body pattern env =
  whenOp pattern env $ \name args ->
    trackDef name (exhaustArgument body env <$> args) env


exhaustArgument :: [Instruction] -> Env -> Argument -> Union
exhaustArgument body env (Argument name types) =
  mconcat $ Set.fromList types :
    maybe mempty (flip map body . exhaustUse env) name


exhaustUse :: Env -> String -> Instruction -> Union
exhaustUse env needle instruction =
  whenOp instruction mempty $ \name args ->
    mconcat . zipWith (exhaustDef needle) args $ getTypes name env


exhaustDef :: String -> Argument -> Union -> Union
exhaustDef needle (Argument haystack _) types =
  if Just needle == haystack then types else mempty


whenOp :: Instruction -> a -> (String -> [Argument] -> a) -> a
whenOp C default_ _ = default_
whenOp (Op name args) _ callback = callback name args


trackDef :: String -> [Union] -> Env -> Env
trackDef =
  Map.insertWith (zipWith Set.union)


toDef :: Env -> OpCode -> Definition
toDef env (OpCode _ code name)=
  Definition name code . fmap Set.toList $ getTypes name env


getTypes :: String -> Env -> [Union]
getTypes name =
  maybe crash id . Map.lookup name
    where crash = errorWithoutStackTrace $ "`" ++ name ++ "` is unbound!"
