import Control.Monad.State (State, evalState)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Monoid ((<>))
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

import qualified Codec.Beam as Beam
import qualified Codec.Beam.Genop as Genop


main :: IO ()
main =
  do  file <- head <$> getArgs
      code <- readFile file
      let name = takeBaseName file
      either print
        (BS.writeFile (name <> ".beam") . Beam.encode (fromString name))
        (compile file code)


compile :: FilePath -> String -> Either ParseError [Beam.Op]
compile file code =
  flip evalState (Env 1 Map.empty Map.empty 0)
    <$> fmap concat
    <$> mapM generate
    <$> parse (contents topLevel) file code



-- CODEGEN


data Env =
  Env
    { _label :: Beam.Label
    , _locals :: Map.Map Name Beam.Register
    , _functions :: Map.Map Name (Beam.Label, Int)
    , _uniqueTmp :: Int
    }


generate :: Def -> State Env [Beam.Op]
generate (Def name args body) =
  do  x <- nextLabel
      y <- nextLabel
      State.modify $ \e -> e
        { _locals = Map.empty
        , _functions = Map.insert name (y, argCount) (_functions e)
        , _uniqueTmp = 0
        }
      headerOps <- sequence $ withArgs genLocal args
      (bodyOps, returnValue) <- genExpr body
      return $
        [ Genop.label x
        , Genop.func_info Beam.Public (fromString (toRaw name)) argCount
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
  do  locals <- State.gets _locals
      let register = Beam.Y (Map.size locals)
      State.modify $ \e -> e { _locals = Map.insert name register locals }
      return $ Genop.move (Beam.Reg destination) register


genExpr :: Expr -> State Env ([Beam.Op], Beam.Operand)
genExpr expr =
  case expr of
    Float f ->
      return ([], Beam.Ext (Beam.EFloat f))

    BinOp operator lhs rhs ->
      genCall (Genop.call_ext "erlang" (stdlibMath operator) 2) [lhs, rhs]

    Var name ->
      lookupVar name >>= \result ->
        return $ case result of
          Left register ->
            ([], Beam.Reg register)

          Right (label, arity) ->
            let bs = fromString (toRaw name) in
            ([Genop.make_fun bs arity label 0], Beam.Reg x0)

    Call name args ->
      lookupVar name >>= \result ->
        case result of
          Left register ->
            do  let fun = Genop.move (Beam.Reg register) (Beam.X (length args))
                (ops, value) <- genCall (Genop.call_fun (length args)) args
                return (fun : ops, value)

          Right (label, _) ->
            genCall (Genop.call (length args) label) args


genCall :: Beam.Op -> [Expr] -> State Env ([Beam.Op], Beam.Operand)
genCall call args =
  do  tmp <- nextTmp
      (argOps, argValues) <- unzip <$> mapM genExpr args
      let ops =
            concat argOps
              ++ withArgs Genop.move argValues
              ++ [call, Genop.move (Beam.Reg x0) tmp]
      return (ops, Beam.Reg tmp)


nextLabel :: State Env Int
nextLabel =
  do  n <- State.gets _label
      State.modify $ \e -> e { _label = n + 1 }
      return n


nextTmp :: State Env Beam.Register
nextTmp =
  do  n <- State.gets $ \e -> _uniqueTmp e + Map.size (_locals e)
      State.modify $ \e -> e { _uniqueTmp = _uniqueTmp e + 1 }
      return (Beam.Y n)


lookupVar :: Name -> State Env (Either Beam.Register (Beam.Label, Int))
lookupVar name =
  do  locals <- State.gets _locals
      functions <- State.gets _functions
      case (Map.lookup name locals, Map.lookup name functions) of
        (Just register, _) -> return $ Left register
        (_, Just funcInfo) -> return $ Right funcInfo
        (Nothing, Nothing) -> error  $ "`" ++ toRaw name ++ "` is undefined!"


withArgs :: (a -> Beam.Register -> op) -> [a] -> [op]
withArgs f list =
  zipWith f list $ map Beam.X [0..]


tmpsNeeded :: Expr -> Int
tmpsNeeded (Float _)         = 0
tmpsNeeded (BinOp _ lhs rhs) = 1 + tmpsNeeded lhs + tmpsNeeded rhs
tmpsNeeded (Var _)           = 0
tmpsNeeded (Call _ args)     = 1 + sum (map tmpsNeeded args)


stdlibMath :: Op -> BS.ByteString
stdlibMath Plus   = "+"
stdlibMath Minus  = "-"
stdlibMath Times  = "*"
stdlibMath Divide = "/"


x0 :: Beam.Register
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
