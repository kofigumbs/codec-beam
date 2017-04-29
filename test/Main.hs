module Main where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put (runPut)
import System.Process (readProcess)

import Test.Framework (Test, buildTest, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)

import qualified Codec.Beam as Beam


runnerFileName :: FilePath
runnerFileName =
  "runner.erl"


testFileName :: FilePath
testFileName =
  "test.beam"


getChunk :: String -> IO String
getChunk chunkName =
  readProcess "escript" [runnerFileName, testFileName, chunkName] ""


testChunk :: String -> String -> Beam.Module -> Test
testChunk chunkName expectedOutput beam =
  buildTest testIO

  where
    testIO =
      do  BS.writeFile testFileName (runPut (Beam.put beam))

          assertion <-
            assertEqual chunkName expectedOutput <$> getChunk chunkName

          return (testCase chunkName assertion)


tests :: [Test]
tests =
  [ testChunk "atom" "[]" (Beam.empty "test")
  ]


main :: IO ()
main =
  defaultMain tests
