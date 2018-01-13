module Ops
  ( Line(..), Instruction(..), Argument(..), Type(..)
  , downloadUrl, Ops.parse
  ) where

import Data.Functor (($>))
import Data.Char (isSpace)
import Text.Parsec hiding (Line)
import Text.Parsec.String (Parser)


data Line
  = GenericOp String Int
  | SpecificOp String [Type]
  | Transform [Instruction] [Instruction]
  deriving Show


data Instruction
  = C
  | Op String [Argument]
  deriving Show


data Argument
  = NameOnly String
  | TypeOnly Type
  | Complete String Type
  deriving Show


data Type
  = Bif
  | Atom
  | XRegister
  | YRegister
  | FloatRegister
  | Integer
  | WordUntagged
  | Export
  | TupleArity
  | TupleByteOffset
  | StackByteOffset
  | Nil
  | Literal
  | Label
  | VarArgs
  | Union [Type]
  -- TODO: include loader-only types?
  deriving (Eq, Ord, Show)


downloadUrl :: String -> String
downloadUrl version =
  "https://raw.githubusercontent.com/erlang/otp/"
    ++ version
    ++ "/erts/emulator/beam/ops.tab"


parse :: String -> Either ParseError [Line]
parse =
  Text.Parsec.parse (endBy line newline <* eof) "ops.tab" . trimRights


trimRights :: String -> String
trimRights =
  unlines . fmap dropTrailing . lines
  where
    dropTrailing = reverse . dropWhile isSpace . reverse


line :: Parser Line
line =
  choice
    [ skipLine '#'
    , skipLine '%'
    , genericOp
    , transform
    , specificOp
    , newline *> line
    ]


skipLine :: Char -> Parser Line
skipLine c =
  char c *> manyTill anyChar newline *> line


genericOp :: Parser Line
genericOp =
  do  name <- try (opName <* char '/')
      arity <- digit
      pure $ GenericOp name $ read [arity]


specificOp :: Parser Line
specificOp =
  try $ SpecificOp <$> opName <*> many (spaceChar *> type_)


transform :: Parser Line
transform =
  Transform <$> try pattern <*> choice
    [ do  many1 spaceChar
          optional breakLine
          sepBy instruction barSpace
    , pure []
    ]


pattern :: Parser [Instruction]
pattern =
  sepBy1 instruction barSpace <* many1 spaceChar <* string "=>"


instruction :: Parser Instruction
instruction =
  do  name <- opName
      choice
        [ do  char '('
              manyTill anyChar $ char ')'
              pure C
        , Op name <$> many (try (many1 spaceChar *> argument))
        ]


argument :: Parser Argument
argument =
  choice
    [ try $ TypeOnly <$> type_ <* notFollowedBy badTypeEnd
    , do  name <- variable
          choice
            [ fmap (Complete name) (equals *> type_)
            , pure (NameOnly name)
            ]
    ]
  where
    badTypeEnd = equals <|> ignore id alphaNum


type_ :: Parser Type
type_ =
  do  firstType <- try singleType
      otherTypes <- many singleType
      optional $ choice
        [ do  try (string "==")
              ignore many1 (alphaNum <|> char '_')
        , ignore char '?'
        , equals <* choice [ ignore many1 digit, atom ]
        ]
      pure $ foldl combineTypes firstType otherTypes
  where
    singleType = choice
      [ builtIn      $> Bif
      , char 'b'     $> Bif
      , char 'a'     $> Atom
      , oneOf "rx"   $> XRegister
      , char 'y'     $> YRegister
      , char 'l'     $> FloatRegister
      , oneOf "Iiqt" $> Integer
      , oneOf "uoL"  $> WordUntagged
      , char 'e'     $> Export
      , char 'A'     $> TupleArity
      , char 'P'     $> TupleByteOffset
      , char 'Q'     $> StackByteOffset
      , char 'n'     $> Nil
      , char 'q'     $> Literal
      , oneOf "fpj"  $> Label
      , char '*'     $> VarArgs
      , char 'c'     $> Union [Atom, Integer, Nil, Literal]
      , char 's'     $> Union [XRegister, YRegister, Literal]
      , oneOf "Sd"   $> Union [XRegister, YRegister]
      ]


builtIn :: Parser ()
builtIn =
  try (string "u$") *> ignore many (alphaNum <|> oneOf "_:/")


atom :: Parser ()
atom =
  ignore choice
    [ try $ string "am_undefined"
    , try $ string "am_true"
    ]


opName :: Parser String
opName =
  many1 (lower <|> digit <|> char '_')


variable :: Parser String
variable =
  do  start <- upper
      rest <- many alphaNum
      pure (start : rest)


breakLine :: Parser ()
breakLine =
  char '\\' *> spaces


barSpace :: Parser ()
barSpace =
  try (string " | ") *> optional breakLine


spaceChar :: Parser ()
spaceChar =
  ignore char ' '


equals :: Parser ()
equals =
  ignore char '='


ignore :: (a -> Parser b) -> a -> Parser ()
ignore toParser a =
  toParser a $> ()


combineTypes :: Type -> Type -> Type
combineTypes (Union lefts) (Union rights) = Union (lefts ++ rights)
combineTypes (Union lefts) right          = Union (right : lefts)
combineTypes left          (Union rights) = Union (left : rights)
combineTypes left          right          = Union [left, right]
