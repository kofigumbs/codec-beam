import System.Directory (listDirectory)

import Test.Hspec

import qualified Build.Generate
import qualified Build.Parse
import qualified Build.Inference


main :: IO ()
main =
  hspec $ mapM_ runBuild =<< runIO (listDirectory fixtureDir)


runBuild :: FilePath -> SpecWith (Arg Expectation)
runBuild dir =
  do  ops      <- get "ops.tab"
      genop    <- get "genop.tab"
      expected <- get "expected.hs"
      it dir $ (`shouldBe` Right expected) $
        Build.Generate.code dir <$>
          do Build.Inference.run <$> Build.Parse.ops ops <*> Build.Parse.genop genop
  where
    get file = runIO $ readFile $ fixtureDir ++ dir ++ "/" ++ file


fixtureDir :: FilePath
fixtureDir =
  "fixtures/"
