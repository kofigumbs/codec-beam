import Control.Monad (liftM2)
import System.Directory (listDirectory)

import Test.Hspec
import qualified Test.Hspec.Expectations as Expect

import qualified Build.Generate
import qualified Build.Parse
import qualified Build.Inference


main :: IO ()
main =
  hspec $ mapM_ makeTest =<< runIO (listDirectory "fixtures")


makeTest :: FilePath -> Spec
makeTest dir =
  do  (ops, genop, expected) <- runIO $ getFixtures dir
      it dir $ matches expected $ Build.Generate.code dir <$>
        liftM2 Build.Inference.run (Build.Parse.ops ops) (Build.Parse.genop genop)


matches :: (Show e, Eq e) => String -> Either e String -> Expect.Expectation
matches expected actual =
  Expect.shouldBe actual (Right (init expected))


getFixtures :: FilePath -> IO (String, String, String)
getFixtures dir =
  let get f = readFile ("fixtures/" ++ dir ++ f) in
  (,,) <$> get "/ops.tab" <*> get "/genop.tab" <*> get "/expected.hs"
