module Main where

import Data.Monoid ((<>))
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import System.FilePath ((</>), (<.>))
import System.Process (callProcess)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as B

import Prelude hiding (unlines)

import qualified Codec.Beam as Beam


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


test :: BS.ByteString -> [BS.ByteString] -> [Beam.Op] -> IO BS.ByteString
test name body ops =
  do  let fixture =
            erlangDir </> toString name <.> "beam"

          (builder, env) =
            Beam.encode (Beam.new name [("test", 0)]) ops

      BS.writeFile fixture (Beam.summarize env (B.toLazyByteString builder))

      return $
        unlines
          [ name <> "_test() ->"
          , "{ok,BEAM} = file:read_file(\"" <> fromString fixture <> "\"),"
          , unlines body <> "."
          ]


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
    [ test "empty"
        [ "?assertMatch("
        , "  {ok, {empty, [{imports, []},{labeled_exports, []},{labeled_locals, []}]}},"
        , "  beam_lib:chunks(BEAM, [imports, labeled_exports, labeled_locals]))"
        ]
        []

    , test "module_atom"
        [ "?assertMatch("
        , "  {ok, {module_atom, [{atoms, [{1,module_atom}]}]}},"
        , "  beam_lib:chunks(BEAM, [atoms]))"
        ]
        []

    , test "constant_function"
        [ "{module, constant_function} ="
        , "  code:load_binary(constant_function, \"constant_function.beam\", BEAM),"
        , "?assertEqual(hello, constant_function:test())"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo "test" 0
        , Beam.Label 2
        , Beam.Move (Beam.Atom "hello") (Beam.X 0)
        , Beam.Return
        ]
    ]
