module Main where

import qualified Data.ByteString.Lazy as BS
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.Process (callProcess)
import System.FilePath ((</>), (<.>))

import qualified Codec.Beam as Beam


erlangDir :: FilePath
erlangDir =
  "test"


erlangModuleName :: String
erlangModuleName =
  "codec_tests"


eunit :: (BS.ByteString, String, [Beam.Instruction]) -> IO String
eunit (name, body, beam) =
  do  let stringName =
            unpack (decodeUtf8 name)

          fixture =
            erlangDir </> stringName <.> "beam"

      BS.writeFile fixture (Beam.encode name beam)

      return $
        unlines
          [ stringName ++ "_test() ->"
          , "{ok,BEAM} = file:read_file(\"" ++ fixture ++ "\"),"
          , body ++ "."
          ]


run :: [String] -> IO ()
run functions =
  do  let fileContents =
            unlines $
              ("-module(" ++ erlangModuleName ++ ").")
                : "-include_lib(\"eunit/include/eunit.hrl\")."
                : functions

          fileName =
            erlangDir </> erlangModuleName <.> "erl"

      writeFile fileName fileContents

      callProcess "erlc" [fileName]

      callProcess "erl"
        [ "-noshell", "-pa", erlangDir
        , "-eval", "eunit:test(" ++ erlangModuleName ++ ", [verbose])"
        , "-run", "init", "stop"
        ]


main :: IO ()
main =
  run =<< mapM eunit
    [ ( "empty"
      , "?assertMatch(\
          \ {ok, {empty, [{imports, []},{labeled_exports, []},{labeled_locals, []}]}},\
          \ beam_lib:chunks(BEAM, [imports, labeled_exports, labeled_locals]))"
      , []
      )

    , ( "module_atom"
      , "?assertMatch(\
          \ {ok, {module_atom, [{atoms, [{1,module_atom}]}]}},\
          \ beam_lib:chunks(BEAM, [atoms]))"
      , []
      )

    , ( "constant_function"
      , unlines
          [ "{module, constant_function} = code:load_binary(constant_function, BEAM),"
          , "?assertEqual(hello, constant_function:function())"
          ]
      , [ Beam.Label
        , Beam.FuncInfo "constant_function" 0
        , Beam.Move (Beam.Atom "hello") (Beam.X 0)
        , Beam.Return
        ]
      )
    ]
