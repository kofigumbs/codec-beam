import Control.Monad.State (State, evalState)
import Data.Monoid ((<>))
import Data.Map.Lazy (Map, (!))
import System.FilePath (takeBaseName)
import System.Environment (getArgs)
import qualified Control.Monad.State as State
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Lazy as Map

import Text.Parsec hiding (State)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import ByteStringConversion (fromString)
import qualified Codec.Beam as Beam
import qualified Codec.Beam.Genop as Genop


main :: IO ()
main =
  do  file <- head <$> getArgs
      code <- readFile file
      let name = takeBaseName file
      either print
        (BS.writeFile (name <> ".beam") . Beam.encode name)
        (compile code)


compile :: String -> Either ParseError [Beam.Op]
compile code =
  flip evalState (Env 1 mempty mempty)
    <$> fmap concat
    <$> mapM generate
    <$> parse (contents topLevel) "KALEIDOSCOPE" code



-- CODEGEN


data Env =
  Env
    { _label :: Beam.Label
    , _vars :: Map Name Beam.Register
    , _functions :: Map Name Beam.Label
    }


generate :: Def -> State Env [Beam.Op]
generate (Def name args body) =
  do  State.modify $ \e -> e { _vars = mempty }
      header <- genFunction name args
      (ops, returnValue) <- genExpr body
      return $ header ++ ops ++
        [ Genop.move returnValue (Beam.X 0)
        , Genop.deallocate (length args)
        , Genop.return_
        ]


genFunction :: Name -> [Name] -> State Env [Beam.Op]
genFunction name@(Name rawName) args =
  do  x <- nextLabel
      y <- nextLabel
      State.modify $ \e -> e { _functions = Map.insert name y (_functions e) }
      allocates <- mapM storeVar args
      let numLive = length args
      return
        $ Genop.label x
        : Genop.func_info Beam.Public (fromString rawName) numLive
        : Genop.label y
        : Genop.allocate numLive numLive -- naive memory allocation
        : allocates


genExpr :: Expr -> State Env ([Beam.Op], Beam.Operand)
genExpr expr =
  case expr of
    Float f ->
      return ([], Beam.Ext (Beam.EFloat f))

    BinOp _op _lhs _rhs ->
      error "TODO"

    Var name ->
      do  vars <- State.gets _vars
          return ([], Beam.Reg (vars ! name))

    Call name args ->
      do  (ops, values) <- unzip <$> mapM genExpr args
          functions <- State.gets _functions
          let moves = zipWith (\v i -> Genop.move v (Beam.X i)) values [0..]
              call = Genop.call (length args) (functions ! name)
          return (concat ops ++ moves ++ [call], Beam.Reg (Beam.X 0))


nextLabel :: State Env Int
nextLabel =
  do  x <- State.gets _label
      State.modify $ \e -> e { _label = x + 1 }
      return x


storeVar :: Name -> State Env Beam.Op
storeVar name =
  do  vars <- State.gets _vars
      let index = Map.size vars
          register = Beam.Y index
      State.modify $ \e -> e { _vars = Map.insert name register vars }
      return $ Genop.move (Beam.Reg (Beam.X index)) register



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


topLevel :: Parser [Def]
topLevel =
  many $
    do  reserved "def"
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
    [ try float
    , try integer
    , try call
    , variable
    , parens expr
    ]


call :: Parser Expr
call =
  Call <$> identifier <*> parens (commaSep expr)


integer :: Parser Expr
integer =
  Float . fromInteger <$> Token.integer lexer


float :: Parser Expr
float =
  Float <$> Token.float lexer


variable :: Parser Expr
variable =
  Var <$> identifier


identifier :: Parser Name
identifier =
  Name <$> Token.identifier lexer


parens :: Parser a -> Parser a
parens =
  Token.parens lexer


commaSep :: Parser a -> Parser [a]
commaSep =
  Token.commaSep lexer


reserved :: String -> Parser ()
reserved =
  Token.reserved lexer


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
