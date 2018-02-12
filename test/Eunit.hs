module Eunit (Test, run, test) where

import Data.Monoid ((<>))
import System.FilePath ((</>), (<.>))
import System.Process (callProcess)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text

import qualified Codec.Beam as Beam


-- Config


erlangDir :: FilePath
erlangDir =
  "test" </> ".eunit"


erlangModuleName :: String
erlangModuleName =
  "codec_tests"



-- Create and run an Eunit test file


type Test =
  IO String


test :: String -> [Beam.Metadata] -> [String] -> [Beam.Op] -> Test
test name metadata body =
  test_ name body . Beam.encode (Text.pack name) metadata


test_ :: String -> [String] -> BS.ByteString -> Test
test_ name body code =
  do  let fixture =
            erlangDir </> name <.> "beam"

      BS.writeFile fixture code

      return $ name <> "_test() ->\n" <> unlines body <> "."


run :: [Test] -> IO ()
run tests =
  do  functions <-
        sequence tests

      let fileContents =
            unlines $
              "-module(" <> erlangModuleName <> ")."
                : "-include_lib(\"eunit/include/eunit.hrl\")."
                : functions

          filename =
            erlangDir </> erlangModuleName <.> "erl"

          runnerCode =
            "case eunit:test(" <> erlangModuleName <> ", [verbose]) of \
            \   error -> init:stop(1); \
            \   _ -> init:stop() \
            \ end."

      writeFile filename fileContents

      callProcess "erlc" ["-o", erlangDir, filename]

      callProcess "erl" ["-noshell", "-pa", erlangDir , "-eval", runnerCode]
