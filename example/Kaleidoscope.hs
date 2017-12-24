import Control.Monad.State (State, evalState)
import Data.Monoid ((<>))
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.FilePath (takeBaseName)
import System.Environment (getArgs)
import qualified Control.Monad.State as State
import qualified Data.ByteString.Lazy as BS

import Text.Parsec hiding (State)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

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
  flip evalState 1
    <$> fmap concat
    <$> mapM generate
    <$> parse (contents topLevel) "KALEIDOSCOPE" code


generate :: Def -> State Beam.Label [Beam.Op]
generate (Def name args body) =
  do  beamHeader <- genFunction name (length args)
      (beamBody, beamReturn) <- genExpr body
      let beamFooter = [Genop.move beamReturn (Beam.X 0), Genop.return_]
      return $ beamHeader ++ beamBody ++ beamFooter


genFunction :: Name -> Int -> State Beam.Label [Beam.Op]
genFunction (Name rawName) numArgs =
  do  x <- nextLabel
      y <- nextLabel
      return
        [ Genop.label x
        , Genop.func_info Beam.Public (encodeUtf8 $ pack rawName) numArgs
        , Genop.label y
        ]


genExpr :: Expr -> State Beam.Label ([Beam.Op], Beam.Operand)
genExpr expr =
  case expr of
    Float f ->
      return ([], Beam.Ext (Beam.EFloat f))

    _ ->
      error "TODO"


nextLabel :: State Beam.Label Int
nextLabel =
  do  x <- State.get
      State.modify (+ 1)
      return x



-- SYNTAX


data Def
  = Def Name [Expr] Expr


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



-- PARSE


topLevel :: Parser [Def]
topLevel =
  many $
    do  reserved "def"
        name <- identifier
        args <- parens $ many variable
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
