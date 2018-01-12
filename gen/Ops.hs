module Ops (Line(..), Type(..), Ops.parse) where

import Data.Either (rights)
import Text.Parsec hiding (Line)
import Text.Parsec.String (Parser)
import qualified Data.Set as Set


data Line
  = GenericOp Int
  | SpecificOp String [Type]
  | Match [Pattern] Body


data Body
  = C
  | Ops [Pattern]


data Pattern =
  Pattern
    { _name :: String
    , _args :: [Argument]
    }


data Argument
  = Named String Type
  | Constrained Type
  | Anonymous String


data Type
  = XRegister
  | YRegister
  | FloatRegister
  | IntTagged
  | IntUntagged
  | WordUntagged
  | Atom
  | Nil
  | Literal
  | Label
  -- TODO: include loader-only types?
  deriving (Eq, Ord, Show)


parse :: String -> Either ParseError [Line]
parse =
  Text.Parsec.parse (sepBy line newline) "ops.tab"


line :: Parser Line
line =
  skipLine '#' <|> skipLine '%' <|> genericOp <|> specificOp <|> pattern


skipLine :: Char -> Parser Line
skipLine c =
  char c >> manyTill anyChar newline >> line


genericOp :: Parser Line
genericOp =
  error "TODO"


specificOp :: Parser Line
specificOp =
  error "TODO"


pattern :: Parser Line
pattern =
  error "TODO"


declaration :: Parser (String, [Set.Set Type])
declaration =
  do  name <- many1 (lower <|> digit <|> char '_')
      args <- many argument
      pure $ (name, args)


argument :: Parser (Set.Set Type)
argument =
  do  firstType <- try (space >> optional variable >> type_)
      otherTypes <- many type_
      optional constraint <|> ignore char '?'
      pure $ Set.fromList (firstType : otherTypes)


type_ :: Parser Type
type_ =
  choice
    [ char 'x' >> pure XRegister
    , char 'y' >> pure YRegister
    , char 'l' >> pure FloatRegister
    , char 'i' >> pure IntTagged
    , char 'q' >> pure IntUntagged
    , char 'u' >> pure WordUntagged
    , char 'L' >> pure WordUntagged
    , char 'a' >> pure Atom
    , char 'n' >> pure Nil
    , char 'q' >> pure Literal
    , char 'c' >> pure Literal
    , char 'f' >> pure Label
    , char 'p' >> pure Label
    , string "Lbl" >> pure Label
    ]

variable :: Parser ()
variable =
  do  many1 upper
      ignore char '='


constraint :: Parser ()
constraint =
  do  ignore char '$'<|> try (ignore string "==")
      ignore many1 $ satisfy (/= ' ')


ignore :: (a -> Parser b) -> a -> Parser ()
ignore toParser a =
  toParser a >> pure ()
