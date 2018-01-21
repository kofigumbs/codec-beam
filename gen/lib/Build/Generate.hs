module Build.Generate (code) where

import Data.Maybe (mapMaybe)
import Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts as H

import Types


code :: String -> [Definition] -> String
code module_ defs =
  H.prettyPrint $
    Module ()
      (Just (ModuleHead () (ModuleName () module_) Nothing (Just (exports defs))))
      []
      [ ImportDecl
          { importAnn = ()
          , importModule = ModuleName () "Codec.Beam.Internal.Types"
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
  ExportSpecList () . map (EVar () . UnQual () . definitionName)


definition :: Definition -> [Decl ()]
definition def@(Definition beamName beamCode beamArgs) =
  TypeSig () [definitionName def] (signature beamName beamArgs)
    : H.sfun (definitionName def) argNames body Nothing
    : concat (imap (typeClass beamName) beamArgs)
  where
    argNames =
      H.genNames "a" (length beamArgs)

    body =
      UnGuardedRhs () $ applyOp beamCode $
        zipWith H.app (imap (extractor beamName) beamArgs) (map H.var argNames)


signature :: String -> [[Types.Type]] -> H.Type ()
signature beamName =
   uncurry applyConstraints . unzip . imap (argument beamName)


applyOp :: Int -> [Exp ()] -> Exp ()
applyOp opCode args =
  H.appFun (H.var opName) [H.intE (fromIntegral opCode), H.listE args]


argument :: String -> Int -> [Types.Type] -> (Maybe (Asst ()), H.Type ())
argument beamName index beamArg =
  case beamArg of
    [type_] ->
      (Nothing, TyVar () (H.name (srcType type_)))

    _ ->
      ( Just $
          ClassA ()
            (UnQual () (constraintName index beamName))
            [TyVar () (genericArgumentName index)]
      , TyVar () (genericArgumentName index)
      )


applyConstraints :: [Maybe (Asst ())] -> [H.Type ()] -> H.Type ()
applyConstraints maybeAssertions types =
  case mapMaybe id maybeAssertions of
    [] ->
      concrete

    assertions ->
      TyForall () Nothing (Just (CxTuple () assertions)) concrete
  where
    concrete = foldr (TyFun ()) (TyVar () opName) types


extractor :: String -> Int -> [Types.Type] -> Exp ()
extractor beamName index beamArg =
  case beamArg of
    [type_] ->
      Var () $ UnQual () (encoderName type_)

    _ ->
      Var () $ UnQual () (methodName index beamName)


typeClass :: String -> Int -> [Types.Type] -> [Decl ()]
typeClass beamName index beamArg =
  if length beamArg <= 1 then
    []
  else
    ClassDecl () Nothing
      (DHApp ()
        (DHead () (constraintName index beamName))
        (UnkindedVar () (genericArgumentName index)))
      []
      (Just
        [ ClsDecl () $ TypeSig () [methodName index beamName] $
            TyFun () (TyVar () (genericArgumentName index)) (TyVar () encodingName)
        ])
      : map (typeInstance beamName index) beamArg


typeInstance :: String -> Int -> Types.Type -> Decl ()
typeInstance beamName index type_ =
  InstDecl () Nothing
    (IRule () Nothing Nothing $
      IHApp ()
        (IHCon () (UnQual () (constraintName index beamName)))
        (TyVar () (H.name (srcType type_)))) $
    Just
      [ InsDecl () $
          H.sfun (methodName index beamName) []
            (UnGuardedRhs () (H.var (encoderName type_)))
            Nothing
      ]


srcType :: Types.Type -> String
srcType Import        = "Import"
srcType Atom          = "ByteString"
srcType XRegister     = "X"
srcType YRegister     = "Y"
srcType FloatRegister = "F"
srcType Literal       = "Literal"
srcType Label         = "Label"
srcType Untagged      = "Int"



-- NAMES


definitionName :: Definition -> Name ()
definitionName =
  H.name . _def_name


constraintName :: Int -> String -> Name ()
constraintName index beamName =
  iname "T" index ("__" ++ beamName)


genericArgumentName :: Int -> Name ()
genericArgumentName index =
  iname "a" index ""


methodName :: Int -> String -> Name ()
methodName index beamName =
  iname "fromT" index ("__" ++ beamName)


encoderName :: Types.Type -> Name ()
encoderName type_ =
  H.name ("From" ++ srcType type_)


encodingName :: Name ()
encodingName =
  H.name "Encoding"


opName :: Name ()
opName =
  H.name "Op"



-- INDEX UTILITIES


iname :: String -> Int -> String -> Name ()
iname prefix index suffix =
  H.name (prefix ++ show index ++ suffix)


imap :: (Int -> a -> b) -> [a] -> [b]
imap f =
  zipWith f [1..]
