module Eunit
  ( run
  , test
  , testMany
  , testConstant
  , testConstant_
  , testCmp
  ) where


import Data.Monoid ((<>))
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import System.FilePath ((</>), (<.>))
import System.Process (callProcess)
import qualified Data.ByteString.Lazy as BS

import Prelude hiding (unlines)
import BShow

import qualified Codec.Beam as Beam
import qualified Codec.Beam.Genop as Beam


-- Helpers


erlangDir :: FilePath
erlangDir =
  "test" </> ".eunit"


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



-- Create and run an Eunit test file


type Test =
  IO BS.ByteString


testConstant_ :: BS.ByteString -> Beam.Operand -> BS.ByteString -> Test
testConstant_ name term =
  testConstant name (const term)


testConstant :: BShow a => BS.ByteString -> (a -> Beam.Operand) -> a -> Test
testConstant name toOperand value =
  test name
    [ "?assertEqual(" <> bshow value <> ", " <> name <> ":check())"
    ]
    [ Beam.label 1
    , Beam.func_info Beam.Public "check" 0
    , Beam.label 2
    , Beam.move (toOperand value) (Beam.X 0)
    , Beam.return_
    ]


testCmp
  :: BS.ByteString
  -> (Int -> Beam.Operand -> Beam.Operand -> Beam.Op)
  -> [(BS.ByteString, BS.ByteString, Bool)]
  -> Test
testCmp name toOp info =
  test name [body]
    [ Beam.label 1
    , Beam.func_info Beam.Public "check" 2
    , Beam.label 2
    , toOp 3 (Beam.Reg (Beam.X 0)) (Beam.Reg (Beam.X 1))
    , Beam.move (Beam.Atom (bshow True)) (Beam.X 0)
    , Beam.return_
    , Beam.label 3
    , Beam.move (Beam.Atom (bshow False)) (Beam.X 0)
    , Beam.return_
    ]

  where
    body =
      BS.intercalate ",\n" $ map line info

    line (first, second, pass) =
      "?assertEqual(" <> bshow pass <> ", "
      <> name <> ":check(" <> first <> ", " <> second <> "))"


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
