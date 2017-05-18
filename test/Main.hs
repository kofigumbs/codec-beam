module Main where

import Data.Monoid ((<>))
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import System.FilePath ((</>), (<.>))
import System.Process (callProcess)
import qualified Data.ByteString.Lazy as BS

import Prelude hiding (unlines)

import qualified Codec.Beam as Beam


-- Helpers


erlangDir :: FilePath
erlangDir =
  "test"


erlangModuleName :: String
erlangModuleName =
  "codec_tests"


unlines :: [BS.ByteString] -> BS.ByteString
unlines =
  BS.intercalate "\n"


toString :: BS.ByteString -> String
toString =
  unpack . decodeUtf8


fromString :: String -> BS.ByteString
fromString =
  encodeUtf8 . pack


fixtureName :: BS.ByteString -> FilePath
fixtureName moduleName =
  erlangDir </> toString moduleName <.> "beam"



-- Create and run an Eunit test file


type Test =
  BS.ByteString -> [BS.ByteString] -> [Beam.Op] -> IO BS.ByteString


testFile :: Test
testFile name body =
  let
    file =
      "File = '" <> fromString (erlangDir </> toString name) <> "',"
  in
    test name (file : body)


testModule :: Test
testModule name body =
  let
    load =
      "c:l(" <> name <> "),"
  in
    test name (load : body)


test :: Test
test name body ops =
  do  let fixture =
            erlangDir </> toString name <.> "beam"

      BS.writeFile fixture (Beam.encode name ops)

      return $ name <> "_test() ->\n" <> unlines body <> "."


run :: [BS.ByteString] -> IO ()
run functions =
  do  let fileContents =
            unlines $
              "-module(" <> fromString erlangModuleName <> ")."
                : "-include_lib(\"eunit/include/eunit.hrl\")."
                : functions

          fileName =
            erlangDir </> erlangModuleName <.> "erl"

      BS.writeFile fileName fileContents

      callProcess "erlc" [fileName]

      callProcess "erl"
        [ "-noshell", "-pa", erlangDir
        , "-eval", "eunit:test(" ++ erlangModuleName ++ ", [verbose])"
        , "-run", "init", "stop"
        ]


main :: IO ()
main =
  run =<< sequence
    [ testFile "just_one_atom"
        [ "?assertMatch("
        , "  { ok, { just_one_atom, ["
        , "    {imports, []},{labeled_exports, []},{labeled_locals, []},"
        , "    {atoms, [{1,just_one_atom}]}"
        , "  ]}},"
        , "  beam_lib:chunks(File, ["
        , "    imports,labeled_exports,labeled_locals,"
        , "    atoms"
        , "  ])"
        , ")"
        ]
        []

    , testModule "constant_function"
        [ "?assertEqual(hello, constant_function:test())"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo True "test" 0
        , Beam.Label 2
        , Beam.Move (Beam.Atom "hello") (Beam.X 0)
        , Beam.Return
        ]

    , testModule "identity_function"
        [ "?assertEqual(1023, identity_function:test())"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo True "test" 0
        , Beam.Label 2
        , Beam.Move (Beam.Int 1023) (Beam.X 0)
        , Beam.CallOnly 1 4
        , Beam.Return
        , Beam.Label 3
        , Beam.FuncInfo False "identity" 1
        , Beam.Label 4
        , Beam.Return
        ]

    , testModule "is_nil"
        [ "?assertEqual(yes, is_nil:test([])),"
        , "?assertEqual(no, is_nil:test(23))"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo True "test" 1
        , Beam.Label 2
        , Beam.IsNil 3 (Beam.Reg (Beam.X 0))
        , Beam.Move (Beam.Atom "yes") (Beam.X 0)
        , Beam.Return
        , Beam.Label 3
        , Beam.Move (Beam.Atom "no") (Beam.X 0)
        , Beam.Return
        ]
    ]
