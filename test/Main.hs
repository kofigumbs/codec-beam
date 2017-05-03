module Main where

import qualified Data.ByteString.Lazy as BS
import System.Process (callProcess)
import System.FilePath ((</>), (<.>))

import qualified Codec.Beam.Builder as Beam

import qualified Test.Atoms


erlangDir :: FilePath
erlangDir =
  "test"


erlangModuleName :: String
erlangModuleName =
  "codec_tests"


eunit :: (Beam.Builder, String, String) -> IO String
eunit (beam, name, body) =
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
    [ Test.Atoms.test
    ]
