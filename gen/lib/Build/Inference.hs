module Build.Inference (run) where

import Types

import Data.List (findIndices)
import qualified Data.Map as Map
import qualified Data.Set as Set


run :: [Line] -> [OpCode] -> [Definition]
run lines opCodes =
  toDef (foldr inferLine mempty lines) <$> opCodes



-- PRIVATE STUFF


type Env =
  Map.Map String [Union]

data Union = Union
  { _types :: Set.Set Type
  , _references :: Set.Set (String, Int)
  }

instance Monoid Union where
  mempty = Union Set.empty Set.empty
  mappend (Union t1 r1) (Union t2 r2) = Union (mappend t1 t2) (mappend r1 r2)


inferLine :: Line -> Env -> Env
inferLine (GenericOp _)             = id
inferLine (Transform patterns body) = flip (foldr (inferPattern body)) patterns
inferLine (SpecificOp name types)   = trackDef name (fmap specificUnion types)
  where
    specificUnion = flip Union Set.empty . Set.fromList


inferPattern :: [Instruction] -> Instruction -> Env -> Env
inferPattern body pattern env =
  whenOp pattern env $ \name args ->
    trackDef name (exhaustArgument body <$> args) env


exhaustArgument :: [Instruction] -> Argument -> Union
exhaustArgument body (Argument name types) =
  Union (Set.fromList types) $
    maybe mempty (Set.fromList . flip concatMap body . exhaustUse) name


exhaustUse :: String -> Instruction -> [(String, Int)]
exhaustUse needle instruction =
  whenOp instruction mempty $ \haystack ->
    map ((,) haystack) . findIndices ((== Just needle) . _arg_name)


whenOp :: Instruction -> a -> (String -> [Argument] -> a) -> a
whenOp C default_ _              = default_
whenOp (Op name args) _ callback = callback name args


trackDef :: String -> [Union] -> Env -> Env
trackDef =
  Map.insertWith (zipWith mappend)


toDef :: Env -> OpCode -> Definition
toDef env (OpCode code name) =
  Definition name code $ map Set.toList $ getTypes name env


getTypes :: String -> Env -> [Set.Set Type]
getTypes name env =
  unwrapUnion env <$> Map.findWithDefault crash name env
  where
    crash = errorWithoutStackTrace $ "`" ++ name ++ "` is unbound!"


unwrapUnion :: Env -> Union -> Set.Set Type
unwrapUnion env (Union types references) =
  mconcat $ types : map unwrapReference (Set.toList references)
  where
    unwrapReference (name, index) = getTypes name env !! index
