module Ops (Op(..), Arg(..), Ops.parse) where

import Data.Either (rights)
import Text.Parsec
import Text.Parsec.String (Parser)


data Op =
  Op
    { _name :: String
    , _args :: [Arg]
    }
  deriving Show


data Arg
  = XRegister
  | YRegister
  | FloatRegister
  | IntTagged
  | IntUntagged
  | Atom
  | Nil
  | Literal
  | Label
  -- TODO: include loader only types?
  deriving Show


parse :: String -> [Op]
parse input =
  concat $ rights $ map (Text.Parsec.parse genop "ops.tab") (lines input)


genop :: Parser [Op]
genop =
  do  ops <- sepBy1 declaration (string " | ")
      choice [ eof, ignore string " =>" ]
      return ops


declaration :: Parser Op
declaration =
  do  name <- many1 (lower <|> digit <|> char '_')
      args <- many argument
      return $ Op name args


argument :: Parser Arg
argument =
  do  space
      optional variable
      type_ <- choice
        [ tag 'x' XRegister
        , tag 'y' YRegister
        , tag 'l' FloatRegister
        , tag 'i' IntTagged
        , tag 'q' IntUntagged
        , tag 'a' Atom
        , tag 'n' Nil
        , tag 'q' Literal
        , tag 'f' Label
        , tag 'p' Label
        ]
      optional constraint
      return type_


variable :: Parser ()
variable =
  do  many1 upper
      char '='
      return ()


constraint :: Parser ()
constraint =
  do  choice [ ignore char '$', ignore string "==" ]
      many1 (satisfy (/= ' '))
      return ()


tag :: Char -> Arg -> Parser Arg
tag character value =
  char character >> return value


ignore :: (a -> Parser b) -> a -> Parser ()
ignore toParser a =
  toParser a >> return ()
