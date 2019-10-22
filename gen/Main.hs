module Main where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)


data Bif = Bif
  { _moduleName :: String
  , _functionName :: String
  , _arity :: Int
  , _cFunctionName :: Maybe String
  }
  deriving Show


main :: IO ()
main =
  either print (mapM_ print) =<<
    parse (many bifParser) fileName <$> readFile fileName
  where
    fileName = "gen/bifs.tab"


bifParser :: Parser Bif
bifParser =
  skipEmptyLines $ Bif
    <$> (keyword *> char ' ' *> atom)
    <*> (char ':' *> (quote *> manyTill anyChar quote <|> atom))
    <*> (char '/' *> (digitToInt <$> digit))
    <*> (optionMaybe (many1 (tab <|> char ' ') *> atom))


atom :: Parser String
atom =
  many1 (alphaNum <|> char '_')


keyword :: Parser String
keyword =
  string "bif" <|> string "ubif" <|> string "hbif"


quote :: Parser Char
quote =
  char '\''


skipEmptyLines :: Parser a -> Parser a
skipEmptyLines parser = 
  choice
    [ newline *> skipEmptyLines parser
    , char '#' *> manyTill anyChar newline *> skipEmptyLines parser
    , parser <* newline
    ]
