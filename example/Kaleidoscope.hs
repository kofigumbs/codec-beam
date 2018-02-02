import Control.Monad.State (State, evalState)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.FilePath (takeBaseName)
import System.Environment (getArgs)
import System.Posix.Files (accessModes, setFileMode)
import qualified Control.Monad.State as State
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Lazy as Map

import Text.Parsec hiding (State, label)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import Codec.Beam.Instructions
import qualified Codec.Beam as Beam


main :: IO ()
main =
  do  file <- fmap head getArgs
      code <- readFile file
      either print (makeExecutable (takeBaseName file)) (compile file code)


compile :: FilePath -> String -> Either ParseError [Beam.Op]
compile file code =
  do  defs <- parse (contents (many def)) file code
      let initial = Env Map.empty Map.empty 1 0
          ops     = fmap concat (mapM generate defs)
      return $ evalState ops initial


makeExecutable :: String -> [Beam.Op] -> IO ()
makeExecutable name ops =
  do  let output = name ++ ".beam"
      writeFile output "#!/usr/bin/env escript\n"
      BS.appendFile output $
        Beam.encode (fromString name) [Beam.Export "main" 1] ops
      setFileMode output accessModes



-- CODEGEN


data Env =
  Env
    { _locals :: Map.Map Name Beam.Y
    , _functions :: Map.Map Name (Beam.Label, Int)
    , _uniqueLabel :: Int
    , _uniqueTmp :: Int
    }


data Value
  = Literal Beam.Literal
  | Variable Beam.Y
  | Return


instance Beam.Source Value where
  fromSource (Literal literal)   = Beam.fromSource literal
  fromSource (Variable register) = Beam.fromSource register
  fromSource Return              = Beam.fromSource returnAddress


generate :: Def -> State Env [Beam.Op]
generate (Def name args body) =
  do  begin  <- genBegin name args
      locals <- sequence (withArgs genLocal args)
      result <- genExpr body
      stack  <- usedVars
      return $ concat
        [ begin
        , [ allocate stack (length args) ]
        , locals
        , fst result
        , [ move (snd result) returnAddress ]
        , finish name stack
        ]


genBegin :: Name -> [Name] -> State Env [Beam.Op]
genBegin name args =
  do  x <- nextLabel
      y <- nextLabel
      State.modify $ \e -> e
        { _locals = Map.empty
        , _functions = Map.insert name (y, length args) (_functions e)
        , _uniqueTmp = 0
        }
      return
        [ label x
        , func_info (toBytes name) (length args)
        , label y
        ]


genLocal :: Beam.Source s => Name -> s -> State Env Beam.Op
genLocal name source =
  do  locals <- State.gets _locals
      let register = Beam.Y (Map.size locals)
      State.modify $ \e -> e { _locals = Map.insert name register locals }
      return $ move source register


genExpr :: Expr -> State Env ([Beam.Op], Value)
genExpr expr =
  case expr of
    Float f ->
      return ([], Literal (Beam.Float f))

    BinOp operator left right ->
      do  tmp <- nextTmp
          lhs <- genExpr left
          rhs <- genExpr right
          let bif = bif2 (Beam.Label 0) (stdlibMath operator 2) (snd lhs) (snd rhs) tmp
          return (fst lhs ++ fst rhs ++ [ bif ], Variable tmp)

    Var name ->
      lookupVar name >>= \result ->
        return $ case result of
          Left register ->
            ([], Variable register)

          Right (lbl, arity) ->
            ([ make_fun2 (Beam.Lambda (toBytes name) arity lbl 0) ] , Return)

    Call name args ->
      lookupVar name >>= \result ->
        case result of
          Left fun ->
            do  (ops, value) <- genCall (call_fun (length args)) args
                return (move fun (Beam.X (length args)) : ops, value)

          Right (lbl, _) ->
            genCall (call (length args) lbl) args


genCall :: Beam.Op -> [Expr] -> State Env ([Beam.Op], Value)
genCall usage args =
  do  tmp <- nextTmp
      (ops, values) <- fmap unzip (mapM genExpr args)
      return
        ( concat ops ++ withArgs move values ++ [ usage, move returnAddress tmp ]
        , Variable tmp
        )


nextLabel :: State Env Beam.Label
nextLabel =
  do  n <- State.gets _uniqueLabel
      State.modify $ \e -> e { _uniqueLabel = n + 1 }
      return (Beam.Label n)


nextTmp :: State Env Beam.Y
nextTmp =
  do  n <- State.gets $ \e -> _uniqueTmp e + Map.size (_locals e)
      State.modify $ \e -> e { _uniqueTmp = _uniqueTmp e + 1 }
      return (Beam.Y n)


lookupVar :: Name -> State Env (Either Beam.Y (Beam.Label, Int))
lookupVar name =
  do  locals <- State.gets _locals
      functions <- State.gets _functions
      case (Map.lookup name locals, Map.lookup name functions) of
        (Just register, _) -> return $ Left register
        (_, Just funcInfo) -> return $ Right funcInfo
        (Nothing, Nothing) -> error  $ "`" ++ _raw name ++ "` is undefined!"


usedVars :: State Env Int
usedVars =
  State.gets $ \e -> _uniqueTmp e + Map.size (_locals e)


withArgs :: (a -> Beam.X -> op) -> [a] -> [op]
withArgs f list =
  zipWith f list $ map Beam.X [0..]


finish :: Name -> Int -> [Beam.Op]
finish name stack =
  if _raw name == "main" then
    [ call_ext_last (Beam.Import "erlang" "display" 1) stack ]
  else
    [ deallocate stack, return_ ]


stdlibMath :: Op -> Int -> Beam.Import
stdlibMath Plus   = Beam.Import "erlang" "+"
stdlibMath Minus  = Beam.Import "erlang" "-"
stdlibMath Times  = Beam.Import "erlang" "*"
stdlibMath Divide = Beam.Import "erlang" "/"


returnAddress :: Beam.X
returnAddress =
  Beam.X 0


toBytes :: Name -> BS.ByteString
toBytes =
  fromString . _raw


fromString :: String -> BS.ByteString
fromString =
  encodeUtf8 . pack



-- SYNTAX


data Def
  = Def Name [Name] Expr


data Expr
  = Float Double
  | BinOp Op Expr Expr
  | Var Name
  | Call Name [Expr]


data Op
  = Plus
  | Minus
  | Times
  | Divide


newtype Name
  = Name { _raw :: String }
  deriving (Eq, Ord)



-- PARSE


def :: Parser Def
def =
  do  Token.reserved lexer "def"
      name <- identifier
      args <- parens $ many identifier
      body <- expr
      reservedOp ";"
      return $ Def name args body


contents :: Parser a -> Parser a
contents inner =
  do  Token.whiteSpace lexer
      result <- inner
      eof
      return result


expr :: Parser Expr
expr =
  Expr.buildExpressionParser table factor
  where
    table =
      [ [binary "*" Times Expr.AssocLeft, binary "/" Divide Expr.AssocLeft]
      , [binary "+" Plus  Expr.AssocLeft, binary "-" Minus  Expr.AssocLeft]
      ]
    binary s f assoc =
      Expr.Infix (reservedOp s >> return (BinOp f)) assoc


factor :: Parser Expr
factor =
  choice
    [ parens expr
    , Float . either fromInteger id <$> Token.naturalOrFloat lexer
    , do  name <- identifier
          arguments <- optionMaybe $ parens (Token.commaSep lexer expr)
          return $ maybe (Var name) (Call name) arguments
    ]


identifier :: Parser Name
identifier =
  Name <$> Token.identifier lexer


parens :: Parser a -> Parser a
parens =
  Token.parens lexer


reservedOp :: String -> Parser ()
reservedOp =
  Token.reservedOp lexer


lexer :: Token.TokenParser ()
lexer =
  Token.makeTokenParser $ emptyDef
    { Token.commentLine = "#"
    , Token.reservedOpNames = ["+","*","-",";"]
    , Token.reservedNames = ["def"]
    }
