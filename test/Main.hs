module Main where

import Data.Monoid ((<>))
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
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


fromString :: String -> BS.ByteString
fromString =
  encodeUtf8 . pack


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
      "File = '" <> fromString (erlangDir </> toString name) <> "',"
  in
    test name (file : body)


testConstant_ :: BS.ByteString -> Beam.Term -> BS.ByteString -> Test
testConstant_ name term =
  testConstant name (const term)


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


testEq
  :: BS.ByteString
  -> (Int -> Beam.Term -> Beam.Term -> Beam.Op)
  -> (Bool, Bool, Bool, Bool)
  -> Test
testEq name toOp (first, second, third, fourth) =
  test name
    [ "?assertEqual(" <> bshow first <> ", " <> name <> ":check(2, 3)),"
    , "?assertEqual(" <> bshow second <> ", " <> name <> ":check(2.0, 3)),"
    , "?assertEqual(" <> bshow third <> ", " <> name <> ":check(2.0, 2)),"
    , "?assertEqual(" <> bshow fourth <> ", " <> name <> ":check(2.0, 2.0))"
    ]
    [ Beam.Label 1
    , Beam.FuncInfo "check" 2
    , Beam.Label 2
    , toOp 3 (Beam.Reg (Beam.X 0)) (Beam.Reg (Beam.X 1))
    , Beam.Move (Beam.Atom (bshow True)) (Beam.X 0)
    , Beam.Return
    , Beam.Label 3
    , Beam.Move (Beam.Atom (bshow False)) (Beam.X 0)
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
    , testConstant "arbitrary_atom" Beam.Atom "hello"
    , testConstant "module_name_atom" Beam.Atom "module_name_atom"
    , testConstant_ "constant_nil" Beam.Nil "[]"

    -- Number equality
    , testEq "is_equal" Beam.IsEq (False, False, True, True)
    , testEq "is_not_equal" Beam.IsNe (True, True, False, False)
    , testEq "is_exactly_equal" Beam.IsEqExact (False, False, False, True)
    , testEq "is_not_exactly_equal" Beam.IsNeExact (True, True, True, False)

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

    , test "get_tuple_element"
        [ "?assertEqual(2, get_tuple_element:first({2})),"
        , "?assertEqual(hi, get_tuple_element:second({oh, hi, there}))"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo "first" 1
        , Beam.Label 2
        , Beam.GetTupleElement (Beam.X 0) 0 (Beam.X 0)
        , Beam.Return
        , Beam.Label 3
        , Beam.FuncInfo "second" 1
        , Beam.Label 4
        , Beam.GetTupleElement (Beam.X 0) 1 (Beam.X 0)
        , Beam.Return
        ]

    , test "set_tuple_element"
        [ "?assertEqual({dream, work}, set_tuple_element:make({team, work}))"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo "make" 1
        , Beam.Label 2
        , Beam.SetTupleElement (Beam.Atom "dream") (Beam.X 0) 0
        , Beam.Return
        ]

    , test "put_list"
        [ "?assertEqual([one, 2], put_list:test())"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo "test" 0
        , Beam.Label 2
        , Beam.PutList (Beam.Int 2) Beam.Nil (Beam.X 0)
        , Beam.PutList (Beam.Atom "one") (Beam.Reg (Beam.X 0)) (Beam.X 0)
        , Beam.Return
        ]

    , test "make_a_tuple"
        [ "?assertEqual({one, 2}, make_a_tuple:test())"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo "test" 0
        , Beam.Label 2
        , Beam.PutTuple 2 (Beam.X 0)
        , Beam.Put (Beam.Atom "one")
        , Beam.Put (Beam.Int 2)
        , Beam.Return
        ]
    ]
