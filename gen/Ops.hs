module Ops (Line(..), Instruction(..), Argument(..), Type(..), Ops.parse) where

import Data.Functor (($>))
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
  = XRegister
  | YRegister
  | FloatRegister
  | Integer
  | WordUntagged
  | Atom
  | Nil
  | Literal
  | Label
  | Bif
  | Union [Type]
  -- TODO: include loader-only types?
  deriving (Eq, Ord, Show)


parse :: String -> Either ParseError [Line]
parse =
  Text.Parsec.parse (endBy line newline) "ops.tab"


line :: Parser Line
line =
  choice
    [ skipLine '#'
    , skipLine '%'
    , genericOp
    , transform
    , specificOp
    , newline >> line
    ]


skipLine :: Char -> Parser Line
skipLine c =
  char c >> manyTill anyChar newline >> line


genericOp :: Parser Line
genericOp =
  do  name <- try (opName <* char '/')
      arity <- digit
      pure $ GenericOp name $ read [arity]


specificOp :: Parser Line
specificOp =
  do  name <- opName
      args <- choice [ oneSpace >> sepBy type_ oneSpace, pure [] ]
      pure $ SpecificOp name args


transform :: Parser Line
transform =
  do  left <- try pattern
      optional oneSpace
      optional breakLine
      right <- sepBy (instruction sepBy) barSpace
      pure $ Transform left right


pattern :: Parser [Instruction]
pattern =
  sepBy1 (instruction endBy) barSpace <* string "=>"


instruction :: (Parser Argument -> Parser () -> Parser [Argument]) -> Parser Instruction
instruction separator =
  do  name <- opName
      choice
        [ do  char '('
              manyTill anyChar $ char ')'
              oneSpace
              pure C
        , do  optional oneSpace
              Op name <$> separator argument oneSpace
        ]


argument :: Parser Argument
argument =
  do  var <- optionMaybe variable
      case var of
        Nothing ->
          TypeOnly <$> type_

        Just name ->
          choice
            [ char '=' >> Complete name <$> type_
            , pure (NameOnly name)
            ]


type_ :: Parser Type
type_ =
  do  firstType <- try singleType
      otherTypes <- many singleType
      optional constraint <|> ignore char '?'
      pure $ foldl combineTypes firstType otherTypes
  where
    singleType =
      choice
        [ char 'x'    $> XRegister
        , char 'y'    $> YRegister
        , char 'l'    $> FloatRegister
        , char 'a'    $> Atom
        , char 'n'    $> Nil
        , char 'q'    $> Literal
        , char 'b'    $> Bif
        , char 'c'    $> Union [Atom, Integer, Nil, Literal]
        , char 's'    $> Union [XRegister, YRegister, Literal]
        , oneOf "uL"  $> WordUntagged
        , oneOf "Sd"  $> Union [XRegister, YRegister]
        , oneOf "fpj" $> Label
        , oneOf "Iiq" $> Integer
        ]


constraint :: Parser ()
constraint =
  do  ignore char '$'<|> try (ignore string "==")
      ignore many (alphaNum <|> oneOf "_:/")


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
  char '\\' >> spaces


barSpace :: Parser ()
barSpace =
  char '|' >> oneSpace >> optional breakLine


oneSpace :: Parser ()
oneSpace =
  ignore char ' '


ignore :: (a -> Parser b) -> a -> Parser ()
ignore toParser a =
  toParser a $> ()


combineTypes :: Type -> Type -> Type
combineTypes (Union lefts) (Union rights) = Union (lefts ++ rights)
combineTypes (Union lefts) right          = Union (right : lefts)
combineTypes left          (Union rights) = Union (left : rights)
combineTypes left          right          = Union [left, right]
