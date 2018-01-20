module Build.Inference (run) where

import Types

import qualified Data.Map as Map
import qualified Data.Set as Set


-- OpCode NUMBER NAME
-- Line
--  = Specific TYPES
--  | Transform [Instr] [Instr]...
--  | Generic NAME ..


run :: [Line] -> [OpCode] -> [Definition]
run lines opCodes =
  map (toDef (foldr inferType Map.empty lines)) opCodes


type Env =
  Map.Map String [(Set.Set Type)]


inferType :: Line -> Env -> Env
inferType (SpecificOp name types) =
  Map.insertWith (zipWith Set.union) name (Set.fromList <$> types)
inferType _ =
  undefined


toDef :: Env -> OpCode -> Definition
toDef env (OpCode _ code name)=
  maybe undefined
    (Definition name code . fmap Set.toList)
    (Map.lookup name env)
