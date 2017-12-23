import Data.Monoid ((<>))
import System.FilePath (takeBaseName)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS

import Text.Parsec
import Text.Parsec.Error (ParseError)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import qualified Codec.Beam as Beam
import qualified Codec.Beam.Genop as Genop



-- SYNTAX


data Expr
  = Float Double
  | BinOp Op Expr Expr
  | Var Name
  | Call Name [Expr]
  | Function Name [Expr] Expr


data Op
  = Plus
  | Minus
  | Times
  | Divide


newtype Name
  = Name String



-- PARSE


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
    , try function
    , try call
    , variable
    , parens expr
    ]


function :: Parser Expr
function =
  do  reserved "def"
      name <- identifier
      args <- parens $ many variable
      Function name args <$> expr


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



-- COMPILE


compile :: String -> Either ParseError [Beam.Op]
compile _ =
  Right []


main :: IO ()
main =
  do  file <- head <$> getArgs
      code <- readFile file
      let name = takeBaseName file
      either print
        (BS.writeFile (name <> ".beam") . Beam.encode name)
        (compile code)
