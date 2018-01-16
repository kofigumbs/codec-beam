module Build.Generate (code) where

-- import Data.Char (toUpper)
import Language.Haskell.Exts
import qualified Data.Set as Set
import qualified Language.Haskell.Exts as Haskell

import Types


code :: String -> [Definition] -> String
code module_ defs =
  prettyPrint $
    Module ()
      (Just (ModuleHead () (ModuleName () module_) Nothing (Just (exports defs))))
      []
      [ ImportDecl
          { importAnn = ()
          , importModule = ModuleName () "Codec.Beam.Internal"
          , importQualified = False
          , importSrc = False
          , importSafe = False
          , importPkg = Nothing
          , importAs = Nothing
          , importSpecs = Nothing
          }
      ]
      (concatMap definition defs)


exports :: [Definition] -> ExportSpecList ()
exports =
  ExportSpecList () . map (EVar () . UnQual () . name . _d_title)


definition :: Definition -> [Decl ()]
definition (Definition title args) =
  [ TypeSig () [name title] (opSignature title args)
  , sfun (name title) argNames
      (appRhs (var (name "Op")) (indexedMap (opArgument title) argNames))
      Nothing
  ]
  where
    argNames = genNames "x" (length args)


opSignature :: String -> [Set.Set Types.Type] -> Haskell.Type ()
opSignature title args =
  let
    (constraints, concretes) =
      unzip $ indexedMap (\index types ->
        case Set.toList types of
          [singleType] ->
            (Nothing, typeType singleType)
          _ ->
            let varName = iName index "t" in
            (Just (opConstraint title varName), TyVar () varName)
      ) args
  in
  foldr (TyForall () Nothing) (foldl1 (TyFun ()) concretes) constraints


opConstraint :: String -> Name () -> Context ()
opConstraint title varName =
  CxSingle () (ClassA () (UnQual () (name ("T_" ++ title))) [TyVar () varName])


opArgument :: String -> Int -> Name () -> Exp ()
opArgument defName index argName =
  app (var (name (defName ++ "__" ++ show index))) (var argName)


typeType :: Types.Type -> Haskell.Type ()
typeType Import = TyVar () (name "Import")
typeType Export = TyVar () (name "Export")
typeType Atom = TyVar () (name "ByteString")
typeType XRegister = TyVar () (name "X")
typeType YRegister = TyVar () (name "Y")
typeType FloatRegister = TyVar () (name "FloatRegister")
typeType Literal = TyVar () (name "Literal")
typeType Label = TyVar () (name "Label")
typeType Untagged = TyVar () (name "Int")
typeType VarArgs =
  error "VarArgs should never make it to this point!"


appRhs :: Exp () -> [Exp ()] -> Rhs ()
appRhs f args =
  UnGuardedRhs () (appFun f args)


indexedMap :: (Int -> a -> b) -> [a] -> [b]
indexedMap f =
  zipWith f [1..]


iName :: Int -> String -> Name ()
iName index string =
  name (string ++ show index)
