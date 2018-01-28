import Control.Monad.State (State, evalState)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Monoid ((<>))
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
  do  file <- head <$> getArgs
      code <- readFile file
      either print (makeExecutable (takeBaseName file)) (compile file code)


compile :: FilePath -> String -> Either ParseError [Beam.Op]
compile file code =
  flip evalState (Env Map.empty Map.empty 1 0)
    <$> fmap concat
    <$> mapM generate
    <$> parse (contents (many def)) file code


makeExecutable :: String -> [Beam.Op] -> IO ()
makeExecutable name ops =
  do  let output = name <> ".beam"
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
  fromSource Return              = Beam.fromSource x0


generate :: Def -> State Env [Beam.Op]
generate (Def name args body) =
  do  header <- genHeader stack name args
      locals <- sequence $ withArgs genLocal args
      (body, value) <- genExpr body
      let footer = [ move value x0, deallocate stack, return_ ]
      return $ concat [ header, locals, body, displayMain name value, footer ]
  where
    stack = length args + tmpsNeeded body


genHeader :: Int -> Name -> [Name] -> State Env [Beam.Op]
genHeader stack name args =
  do  x <- nextLabel
      y <- nextLabel
      State.modify $ \e -> e
        { _locals = Map.empty
        , _functions = Map.insert name (y, length args) (_functions e)
        , _uniqueTmp = 0
        }
      return
        [ label x
        , func_info (fromString (toRaw name)) (length args)
        , label y
        , allocate stack (length args)
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

    BinOp operator lhs rhs ->
      genCall (call_ext (Beam.Import "erlang" (stdlibMath operator) 2)) [lhs, rhs]

    Var name ->
      lookupVar name >>= \result ->
        return $ case result of
          Left register ->
            ([], Variable register)

          Right (label, arity) ->
            let bs = fromString (toRaw name) in
            ([make_fun2 (Beam.Lambda bs arity label 0)], Return)

    Call name args ->
      lookupVar name >>= \result ->
        case result of
          Left register ->
            do  let fun = move register (Beam.X (length args))
                (ops, value) <- genCall (call_fun (length args)) args
                return (fun : ops, value)

          Right (label, _) ->
            genCall (call (length args) label) args


genCall :: Beam.Op -> [Expr] -> State Env ([Beam.Op], Value)
genCall call args =
  do  tmp <- nextTmp
      (argOps, argValues) <- unzip <$> mapM genExpr args
      let ops =
            concat argOps
              ++ withArgs move argValues
              ++ [call, move x0 tmp]
      return (ops, Variable tmp)


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
        (Nothing, Nothing) -> error  $ "`" ++ toRaw name ++ "` is undefined!"


withArgs :: (a -> Beam.X -> op) -> [a] -> [op]
withArgs f list =
  zipWith f list $ map Beam.X [0..]


tmpsNeeded :: Expr -> Int
tmpsNeeded (Float _)         = 0
tmpsNeeded (BinOp _ lhs rhs) = 1 + tmpsNeeded lhs + tmpsNeeded rhs
tmpsNeeded (Var _)           = 0
tmpsNeeded (Call _ args)     = 1 + sum (map tmpsNeeded args)


displayMain :: Name -> Value -> [Beam.Op]
displayMain name value =
  if toRaw name == "main" then
    [ move value x0, call_ext (Beam.Import "erlang" "display" 1) ]
  else
    []


stdlibMath :: Op -> BS.ByteString
stdlibMath Plus   = "+"
stdlibMath Minus  = "-"
stdlibMath Times  = "*"
stdlibMath Divide = "/"


x0 :: Beam.X
x0 =
  Beam.X 0


toRaw :: Name -> String
toRaw (Name raw) =
  raw


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
  = Name String
  deriving (Eq, Ord, Show)



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
