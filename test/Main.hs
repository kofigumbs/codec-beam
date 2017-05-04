module Main where

import qualified Data.ByteString.Lazy as BS
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.Process (callProcess)
import System.FilePath ((</>), (<.>))

import qualified Codec.Beam.Builder as Beam


erlangDir :: FilePath
erlangDir =
  "test"


erlangModuleName :: String
erlangModuleName =
  "codec_tests"


eunit :: (BS.ByteString, String, Beam.Code) -> IO String
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
    [ ( "atoms"
      , "?assertMatch(\
          \ {ok, {atoms, [{atoms, [{1,atoms},{2,some_atom}]}]}},\
          \ beam_lib:chunks(BEAM, [atoms]))"
      , do
          Beam.atom "some_atom"
          return []
      )

    , ( "atoms_repeated"
      , "?assertMatch(\
          \ {ok, {atoms_repeated, [{atoms, [{1,atoms_repeated},{2,another_one}]}]}},\
          \ beam_lib:chunks(BEAM, [atoms]))"
      , do
          Beam.atom "atoms_repeated"
          Beam.atom "another_one"
          return []
      )

    , ( "constant_function"
      , unlines
          [ "{module, constant_function} = code:load_binary(constant_function, BEAM),"
          , "?assertEqual(hello, constant_function:function())"
          ]
      , do
          hello <-
            Beam.atom "hello"

          return
            [ (,) "function"
                [ Beam.move (Beam.AOp hello) (Beam.XOp 0)
                , Beam.ret
                ]
            ]
      )
    ]
