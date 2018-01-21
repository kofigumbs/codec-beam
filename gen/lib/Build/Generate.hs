module Build.Generate (code) where

import Data.List (intercalate)
import Data.Either (rights)

import Types


code :: String -> [Definition] -> String
code moduleName defs =
  "module " ++ moduleName ++ " (" ++ sepBy comma _def_name defs ++ ") where\n"
    ++ "import Codec.Beam.Internal.Types\n\n"
    ++ sepBy "\n\n" pp (concatMap definition defs)


-- AST


data TopLevel
  = Class Int String
  | Instance Int String Type
  | Function String Int [Either (Int, Type) Int]


definition :: Definition -> [TopLevel]
definition (Definition name code args) =
  let arguments = indexMap (argument name) args in
  Function name code (fmap fst <$> arguments) : (concatMap snd $ rights arguments)


argument :: String -> Int -> [Type] -> Either (Int, Type) (Int, [TopLevel])
argument _ index [type_]        = Left (index, type_)
argument baseName index types   = Right (index, constraint)
  where
    constraint = Class index baseName : map (Instance index baseName) types


indexMap :: (Int -> a -> b) -> [a] -> [b]
indexMap f =
  zipWith f [1..]



-- PRETTY PRINTING


pp :: TopLevel -> String
pp topLevel =
  case topLevel of
    Class index baseName ->
      "class " ++ className index baseName ++ space
        ++ genericArgumentName index ++ " where\n"
        ++ indent ++ methodName index baseName ++ " :: "
        ++ genericArgumentName index ++ " -> " ++ encodingName

    Instance index baseName type_ ->
      "instance " ++ className index baseName ++ space
        ++ srcType type_ ++ " where\n"
        ++ indent ++ methodName index baseName ++ " = "
        ++ encoderName type_

    Function name opCode args ->
      name ++ " :: " ++ constraints name (rights args)
        ++ sepBy " -> " (either (srcType . snd) genericArgumentName) args
        ++ " -> " ++ opName ++ "\n"
        ++ name ++ space
        ++ sepBy space (either (genericArgumentName . fst) genericArgumentName) args
        ++ " = " ++ opName ++ space ++ show opCode ++ " ["
        ++ sepBy comma (encoding name) args ++ "]"


constraints :: String -> [Int] -> String
constraints _ []             = ""
constraints baseName indexes = "(" ++ sepBy comma class_ indexes ++ ") => "
  where
    class_ i = className i baseName ++ space ++ genericArgumentName i


encoding :: String -> Either (Int, Type) Int -> String
encoding _ (Left (index, type_)) = encoderName type_ ++ nextArgumentName index
encoding baseName (Right index)  = methodName index baseName ++ nextArgumentName index


srcType :: Type -> String
srcType Import        = "Import"
srcType Atom          = "ByteString"
srcType XRegister     = "X"
srcType YRegister     = "Y"
srcType FloatRegister = "F"
srcType Literal       = "Literal"
srcType Label         = "Label"
srcType Untagged      = "Int"


indent :: String
indent =
  replicate 8 ' '


space :: String
space =
  " "


comma :: String
comma =
  ", "


sepBy :: String -> (a -> String) -> [a] -> String
sepBy separator function =
  intercalate separator . map function



-- NAMES


className :: Int -> String -> String
className index beamName =
  "T" ++ show index ++ "__" ++ beamName


nextArgumentName :: Int -> String
nextArgumentName index =
  space ++ genericArgumentName index


genericArgumentName :: Int -> String
genericArgumentName index =
  "a" ++ show index


methodName :: Int -> String -> String
methodName index beamName =
  "fromT" ++ show index ++ "__" ++ beamName


encoderName :: Type -> String
encoderName type_ =
  "From" ++ srcType type_


encodingName :: String
encodingName =
 "Encoding"


opName :: String
opName =
  "Op"
