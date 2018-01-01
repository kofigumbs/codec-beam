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
  flip evalState newEnv
    <$> fmap concat
    <$> mapM generate
    <$> parse (contents topLevel) "KALEIDOSCOPE" code

  where
    newEnv =
      Env { _label = 1, _vars = Map.empty, _functions = Map.empty }



-- CODEGEN


data Env =
  Env
    { _label :: Beam.Label
    , _vars :: Map Variable Beam.Register
    , _functions :: Map Name Beam.Label
    }


data Variable
  = Local Name
  | Tmp


{-| Force 'Tmp' variables to be unique. -}
instance Ord Variable where
  compare Tmp _ = LT
  compare _ Tmp = GT
  compare (Local left) (Local right) = compare left right
instance Eq Variable where
  (==) left right = compare left right == EQ


generate :: Def -> State Env [Beam.Op]
generate (Def name@(Name rawName) args body) =
  do  x <- nextLabel
      y <- nextLabel
      State.modify $ \e -> e
        { _vars = Map.empty
        , _functions = Map.insert name y (_functions e)
        }
      headerOps <- sequence $ withArgs genLocal args
      (bodyOps, returnValue) <- genExpr body
      return $
        [ Genop.label x
        , Genop.func_info Beam.Public (fromString rawName) argCount
        , Genop.label y
        , Genop.allocate spaceNeeded argCount
        ] ++ headerOps ++ bodyOps ++
        [ Genop.move returnValue x0
        , Genop.deallocate spaceNeeded
        , Genop.return_
        ]
  where
    argCount = length args
    spaceNeeded = argCount + tmpsNeeded body


genLocal :: Name -> Beam.Register -> State Env Beam.Op
genLocal name destination =
  Genop.move (Beam.Reg destination) <$> nextVar (Local name)


genExpr :: Expr -> State Env ([Beam.Op], Beam.Operand)
genExpr expr =
  case expr of
    Float f ->
      return ([], Beam.Ext (Beam.EFloat f))

    BinOp operator lhs rhs ->
      genCall (Genop.call_ext (erlangArithmetic operator)) [lhs, rhs]

    Var name ->
      do  vars <- State.gets _vars
          return ([], Beam.Reg (vars ! Local name))

    Call name args ->
      do  functions <- State.gets _functions
          genCall (Genop.call (length args) (functions ! name)) args


genCall :: Beam.Op -> [Expr] -> State Env ([Beam.Op], Beam.Operand)
genCall call args =
  do  tmp <- nextVar Tmp
      (argOps, argValues) <- unzip <$> mapM genExpr args
      let ops =
            concat argOps
              ++ withArgs Genop.move argValues
              ++ [call, Genop.move (Beam.Reg x0) tmp]
      return (ops, Beam.Reg tmp)


nextLabel :: State Env Int
nextLabel =
  do  x <- State.gets _label
      State.modify $ \e -> e { _label = x + 1 }
      return x


nextVar :: Variable -> State Env Beam.Register
nextVar v =
  do  vars <- State.gets _vars
      let register = Beam.Y (Map.size vars)
      State.modify $ \e -> e { _vars = Map.insert v register vars }
      return register


withArgs :: (a -> Beam.Register -> op) -> [a] -> [op]
withArgs f list =
  zipWith f list $ map Beam.X [0..]


tmpsNeeded :: Expr -> Int
tmpsNeeded (Float _)         = 0
tmpsNeeded (BinOp _ lhs rhs) = 1 + tmpsNeeded lhs + tmpsNeeded rhs
tmpsNeeded (Var _)           = 0
tmpsNeeded (Call _ args)     = 1 + sum (map tmpsNeeded args)


erlangArithmetic :: Op -> Beam.Function
erlangArithmetic Plus   = Beam.Function "erlang" "+" 2
erlangArithmetic Minus  = Beam.Function "erlang" "-" 2
erlangArithmetic Times  = Beam.Function "erlang" "*" 2
erlangArithmetic Divide = Beam.Function "erlang" "/" 2


x0 :: Beam.Register
x0 =
  Beam.X 0



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
