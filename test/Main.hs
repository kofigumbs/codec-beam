module Main where

import qualified Data.ByteString.Lazy as BS
import System.Process (callProcess)
import System.FilePath ((</>), (<.>))

import qualified Codec.Beam.Builder as Beam


erlangDir :: FilePath
erlangDir =
  "test"


erlangModuleName :: String
erlangModuleName =
  "codec_tests"


eunit :: (String, String, Beam.Builder) -> IO String
eunit (name, body, beam) =
  do  let fixture =
            erlangDir </> name <.> "beam"

      BS.writeFile fixture (Beam.encode beam)

      return $
        unlines
          [ name ++ "_test() ->"
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
    [ ( "atoms"
      , "?assertMatch(\
          \ {ok, {module_name, [{atoms, [{1,module_name},{2,another_one}]}]}},\
          \ beam_lib:chunks(BEAM, [atoms]))"
      , Beam.withAtom "another_one"
          $ Beam.named "module_name"
      )
    ]
