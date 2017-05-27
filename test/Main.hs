module Main where

import Data.Monoid ((<>))
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.FilePath ((</>), (<.>))
import System.Process (callProcess)
import qualified Data.ByteString.Lazy as BS

import Prelude hiding (unlines)
import BShow

import qualified Codec.Beam as Beam


-- Helpers


erlangDir :: FilePath
erlangDir =
  "test" </> "eunit"


erlangModuleName :: String
erlangModuleName =
  "codec_tests"


unlines :: [BS.ByteString] -> BS.ByteString
unlines =
  BS.intercalate "\n"


toString :: BS.ByteString -> String
toString =
  unpack . decodeUtf8


fixtureName :: BS.ByteString -> FilePath
fixtureName moduleName =
  erlangDir </> toString moduleName <.> "beam"



-- Create and run an Eunit test file


type Test =
  IO BS.ByteString


testFile :: BS.ByteString -> [BS.ByteString] -> [Beam.Op] -> Test
testFile name body =
  let
    file =
      "File = '" <> bshow (erlangDir </> toString name) <> "',"
  in
    test name (file : body)


testConstant :: BShow a => BS.ByteString -> (a -> Beam.Term) -> a -> Test
testConstant name toTerm value =
  test name
    [ "?assertEqual(" <> bshow value <> ", " <> name <> ":test())"
    ]
    [ Beam.Label 1
    , Beam.FuncInfo "test" 0
    , Beam.Label 2
    , Beam.Move (toTerm value) (Beam.X 0)
    , Beam.Return
    ]


test :: BS.ByteString -> [BS.ByteString] -> [Beam.Op] -> Test
test name body ops =
  do  let fixture =
            erlangDir </> toString name <.> "beam"

      BS.writeFile fixture (Beam.encode name ops)

      return $ name <> "_test() ->\n" <> unlines body <> "."


run :: [BS.ByteString] -> IO ()
run functions =
  do  let fileContents =
            unlines $
              "-module(" <> bshow erlangModuleName <> ")."
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



-- Program


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

    -- From beam_asm: https://git.io/vHTBY
    , testConstant "number_five" Beam.Int 5
    , testConstant "number_one_thousand" Beam.Int 5
    , testConstant "number_two_thousand_forty_seven" Beam.Int 2047
    , testConstant "number_two_thousand_forty_eight" Beam.Int 2048
    , testConstant "number_negative_one" Beam.Int (-1)
    , testConstant "number_large_negative" Beam.Int (-4294967295)
    , testConstant "number_large_positive" Beam.Int 4294967295
    , testConstant "number_very_large_positive" Beam.Int 429496729501

    -- Atom table encodings
    , testConstant "constant_nil" (const Beam.Nil) ("[]" :: BS.ByteString)
    , testConstant "arbitrary_atom" Beam.Atom "hello"
    , testConstant "module_name_atom" Beam.Atom "module_name_atom"

    , test "call_into_identity"
        [ "?assertEqual(1023, call_into_identity:test())"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo "test" 0
        , Beam.Label 2
        , Beam.Move (Beam.Int 1023) (Beam.X 0)
        , Beam.CallOnly 1 4
        , Beam.Return
        , Beam.Label 3
        , Beam.FuncInfo "identity" 1
        , Beam.Label 4
        , Beam.Return
        ]

    , test "is_nil"
        [ "?assertEqual(yes, is_nil:test([])),"
        , "?assertEqual(no, is_nil:test(23))"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo "test" 1
        , Beam.Label 2
        , Beam.IsNil 3 (Beam.Reg (Beam.X 0))
        , Beam.Move (Beam.Atom "yes") (Beam.X 0)
        , Beam.Return
        , Beam.Label 3
        , Beam.Move (Beam.Atom "no") (Beam.X 0)
        , Beam.Return
        ]

    -- Based on https://happi.github.io/theBeamBook/#x_and_y_regs_in_memory
    , test "allocate_for_call_fun"
        [ "_add = fun 'erlang':'+'/2,"
        , "?assertEqual(4, allocate_for_call_fun:apply2(2, 2, _add))"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo "apply2" 3
        , Beam.Label 2
        , Beam.Allocate 2 3
        , Beam.Move (Beam.Reg (Beam.X 2)) (Beam.Y 1)
        , Beam.Move (Beam.Reg (Beam.X 1)) (Beam.Y 0)
        , Beam.Call 1 4
        , Beam.Move (Beam.Reg (Beam.X 0)) (Beam.X 1)
        , Beam.Move (Beam.Reg (Beam.Y 0)) (Beam.X 0)
        , Beam.Move (Beam.Reg (Beam.X 1)) (Beam.Y 0)
        , Beam.Call 1 4
        , Beam.Move (Beam.Reg (Beam.Y 1)) (Beam.X 2)
        , Beam.Move (Beam.Reg (Beam.X 0)) (Beam.X 1)
        , Beam.Move (Beam.Reg (Beam.Y 0)) (Beam.X 0)
        , Beam.CallFun 2
        , Beam.Deallocate 2
        , Beam.Return
        , Beam.Label 3
        , Beam.FuncInfo "identity" 1
        , Beam.Label 4
        , Beam.Return
        ]
    ]
