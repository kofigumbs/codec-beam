import System.Directory (listDirectory)

import Test.Hspec

import qualified Build.Generate
import qualified Build.Parse
import qualified Build.Inference


main :: IO ()
main =
  hspec $ mapM_ runBuild =<< runIO (listDirectory "fixtures")


runBuild :: FilePath -> Spec
runBuild dir =
  runIO (getFixtures dir) >>= \(ops, genop, expected) ->
    it dir $ (`shouldBe` Right expected) $
      Build.Generate.code dir <$>
        do Build.Inference.run <$> Build.Parse.ops ops <*> Build.Parse.genop genop


getFixtures :: FilePath -> IO (String, String, String)
getFixtures dir =
  let get f = readFile $ "fixtures/" ++ dir ++ f in
  (,,) <$> get "/ops.tab" <*> get "/genop.tab" <*> get "/expected.hs"
