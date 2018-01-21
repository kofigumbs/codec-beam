import Data.Bifunctor (first)
import Network.Curl.Download (openURIString)
import System.Environment (getArgs)

import qualified Build.Generate
import qualified Build.Parse
import qualified Build.Inference


main :: IO ()
main =
  do  version <- getVersion <$> getArgs
      rawOps <- download version "/erts/emulator/beam/ops.tab"
      rawGenop <- download version "/lib/compiler/src/genop.tab"
      either errorWithoutStackTrace id $
        writeFile "src/Codec/Beam/Internal/Generated.hs" <$>
          Build.Generate.code "Codec.Beam.Internal.Generated" <$>
            do Build.Inference.run <$> run Build.Parse.ops rawOps <*> run Build.Parse.genop rawGenop


getVersion :: [String] -> String
getVersion args =
  let context = " Erlang SHA|VERSION|BRANCH command-line argument!" in
  case args of
    [version]  -> version
    []         -> errorWithoutStackTrace $ "Missing" ++ context
    _          -> errorWithoutStackTrace $ "Ambigious" ++ context


download :: String -> String -> IO (Either String String)
download version path =
  openURIString (rootUrl ++ version ++ path)
  where
    rootUrl = "https://raw.githubusercontent.com/erlang/otp/"


run :: Show e => (a -> Either e b) -> Either String a -> Either String b
run parse rawInput =
  first show <$> parse =<< rawInput
