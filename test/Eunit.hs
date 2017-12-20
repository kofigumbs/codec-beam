module Eunit (Test, run , test , testMany) where

import Data.Monoid ((<>))
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import System.FilePath ((</>), (<.>))
import System.Process (callProcess)
import qualified Data.ByteString.Lazy as BS

import Prelude hiding (unlines)

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
  IO BS.ByteString


testMany
  :: BS.ByteString
  -> [BS.ByteString]
  -> [[Beam.Op]]
  -> Test
testMany name body =
  test_ name body . Beam.toLazyByteString . foldl Beam.append (Beam.new name)


test :: BS.ByteString -> [BS.ByteString] -> [Beam.Op] -> Test
test name body =
  test_ name body . Beam.encode name


test_ :: BS.ByteString -> [BS.ByteString] -> BS.ByteString -> Test
test_ name body code =
  do  let fixture =
            erlangDir </> toString name <.> "beam"

      BS.writeFile fixture code

      return (name <> "_test() ->\n" <> unlines body <> ".")


run :: [Test] -> IO ()
run tests =
  do  functions <-
        sequence tests

      let fileContents =
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



-- Helpers


unlines :: [BS.ByteString] -> BS.ByteString
unlines =
  BS.intercalate "\n"


toString :: BS.ByteString -> String
toString =
  unpack . decodeUtf8


fromString :: String -> BS.ByteString
fromString =
  encodeUtf8 . pack
