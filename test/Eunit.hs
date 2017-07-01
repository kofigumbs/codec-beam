module Eunit
  ( run
  , test
  , testMany
  , testConstant
  , testConstant_
  , testEq
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


-- Helpers


erlangDir :: FilePath
erlangDir =
  "test" </> "eunit"


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
    [ "?assertEqual(" <> bshow value <> ", " <> name <> ":test())"
    ]
    [ Beam.Label 1
    , Beam.FuncInfo Beam.Public "test" 0
    , Beam.Label 2
    , Beam.Move (toOperand value) (Beam.X 0)
    , Beam.Return
    ]


testEq
  :: BS.ByteString
  -> (Int -> Beam.Operand -> Beam.Operand -> Beam.Op)
  -> (Bool, Bool, Bool, Bool)
  -> Test
testEq name toOp (first, second, third, fourth) =
  test name
    [ "?assertEqual(" <> bshow first <> ", " <> name <> ":check(2, 3)),"
    , "?assertEqual(" <> bshow second <> ", " <> name <> ":check(2.0, 3)),"
    , "?assertEqual(" <> bshow third <> ", " <> name <> ":check(2.0, 2)),"
    , "?assertEqual(" <> bshow fourth <> ", " <> name <> ":check(2.0, 2.0))"
    ]
    [ Beam.Label 1
    , Beam.FuncInfo Beam.Public "check" 2
    , Beam.Label 2
    , toOp 3 (Beam.Reg (Beam.X 0)) (Beam.Reg (Beam.X 1))
    , Beam.Move (Beam.Atom (bshow True)) (Beam.X 0)
    , Beam.Return
    , Beam.Label 3
    , Beam.Move (Beam.Atom (bshow False)) (Beam.X 0)
    , Beam.Return
    ]


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
