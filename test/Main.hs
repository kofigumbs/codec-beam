module Main where

import qualified Data.ByteString.Lazy as BS
import System.Process (readProcess)
import System.FilePath ((</>), (<.>))
import Control.Exception (try, IOException)

import Test.Framework (Test, buildTest, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)

import qualified Codec.Beam as Beam


runnerFileName :: FilePath
runnerFileName =
  "test" </> "runner" <.> "erl"


testFileName :: FilePath
testFileName =
  "test" </> "module" <.> "beam"


showError :: IOException -> String
showError =
  show


getChunk :: String -> IO String
getChunk chunkName =
  let
    args =
      [runnerFileName, testFileName, chunkName]
  in
    either showError id <$> try (readProcess "escript" args "")


testChunk :: String -> String -> Beam.Module -> Test
testChunk chunkName expectedOutput beam =
  buildTest testIO

  where
    testIO =
      do  BS.writeFile testFileName (Beam.encode beam)

          assertion <-
            assertEqual chunkName expectedOutput <$> getChunk chunkName

          return (testCase chunkName assertion)


tests :: [Test]
tests =
  [ testChunk "atoms" "[]" (Beam.empty "module")
  ]


main :: IO ()
main =
  defaultMain tests
