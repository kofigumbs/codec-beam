module Build.Parse (genop, ops) where

import Data.Functor (($>))
import Data.Char (isSpace)
import Text.Parsec hiding (Line)
import Text.Parsec.String (Parser)

import Types


genop :: String -> Either ParseError [OpCode]
genop =
  parse (many opCode <* eof) "genop.tab" . trimRights


ops :: String -> Either ParseError [Line]
ops =
  parse (many line <* eof) "ops.tab" . trimRights


trimRights :: String -> String
trimRights =
  unlines . map dropTrailing . lines
  where
    dropTrailing = reverse . dropWhile isSpace . reverse


opCode :: Parser OpCode
opCode =
  skipEmptys $ choice
    [ string "BEAM_FORMAT_NUMBER=0" *> newline *> opCode
    , do  code <- read <$> many1 digit
          char ':'
          whitespace
          deprecated <- choice [ char '-' $> True, pure False ]
          name <- opName
          char '/'
          digit
          newline
          pure $ OpCode deprecated code name
    ]


line :: Parser Line
line =
  skipEmptys (genericOp <|> transform <|> specificOp)


skipEmptys :: Parser a -> Parser a
skipEmptys parser =
  choice
    [ skipLine '#' *> skipEmptys parser
    , skipLine '%' *> skipEmptys parser
    , newline      *> skipEmptys parser
    , parser
    ]


skipLine :: Char -> Parser ()
skipLine character =
  ignore char character <* manyTill anyChar newline


genericOp :: Parser Line
genericOp =
  GenericOp <$> try (opName <* char '/') <* digit <* newline


specificOp :: Parser Line
specificOp =
  SpecificOp <$> opName <*> many (whitespace *> specificType) <* newline


transform :: Parser Line
transform =
  Transform <$> try pattern <*> choice
    [ newline *> pure []
    , whitespace *> sepBy (instruction rightArg) barSep <* newline
    ]


pattern :: Parser [Instruction]
pattern =
  sepBy1 (instruction leftArg) barSep <* whitespace <* string "=>"


instruction :: Parser Argument -> Parser Instruction
instruction argument =
  do  name <- opName
      choice
        [ do  char '('
              manyTill anyChar $ char ')'
              pure C
        , Op name <$> many (try (whitespace *> argument))
        ]


leftArg :: Parser Argument
leftArg =
  choice
    [ TypeOnly <$> patternType
    , do  name <- variable
          choice
            [ fmap (Complete name) (char '=' *> patternType)
            , pure (NameOnly name)
            ]
    ] <* optional constraint
  where
    constraint = string "==" *> many1 (alphaNum <|> char '_')


rightArg :: Parser Argument
rightArg =
  choice
    [ fmap TypeOnly (patternType <* optional default_)
    , fmap NameOnly variable
    ]
  where
    default_ = char '=' *> choice [ ignore many1 digit, atom ]


patternType :: Parser [Type]
patternType =
  choice
    [ char '*' $> [VarArgs]
    , builtIn  $> [Import]
    , lookAhead lower *> specificType
    ]


specificType :: Parser [Type]
specificType =
  foldl (++) <$> try singleType <*> many singleType
  where
    singleType = choice
      [ char 'b'         $> [Import]
      , char 'e'         $> [Export]
      , char 'a'         $> [Atom]
      , oneOf "rx"       $> [XRegister]
      , char 'y'         $> [YRegister]
      , char 'l'         $> [FloatRegister]
      , oneOf "inq"      $> [Literal]
      , oneOf "fjp"      $> [Label]
      , oneOf "touAILPQ" $> [Untagged]
      , char 'c'         $> [Atom, Literal]
      , char 's'         $> [XRegister, YRegister, Atom, Literal]
      , oneOf "dS"       $> [XRegister, YRegister]
      ]


builtIn :: Parser ()
builtIn =
  try (string "u$") *> ignore many (alphaNum <|> oneOf "_:/")


atom :: Parser ()
atom =
  try (string "am_") *> ignore many1 (lower <|> char '_')


opName :: Parser String
opName =
  many1 (lower <|> digit <|> char '_')


variable :: Parser String
variable =
  (:) <$> upper <*> many alphaNum


barSep :: Parser ()
barSep =
  try (whitespace *> char '|') *> whitespace


whitespace :: Parser ()
whitespace =
  many1 (char ' ') *> optional (char '\\' <* spaces)


ignore :: (a -> Parser b) -> a -> Parser ()
ignore toParser a =
  toParser a $> ()
