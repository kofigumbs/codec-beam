import Data.Bifunctor (first)
import Network.Curl.Download (openURIString)
import System.Environment (getArgs)
import qualified Language.Haskell.Exts as Haskell

import qualified Code
import qualified Parse


main :: IO ()
main =
  do  version <- getVersion <$> getArgs
      rawOps <- download version "/erts/emulator/beam/ops.tab"
      rawGenop <- download version "/lib/compiler/src/genop.tab"
      either errorWithoutStackTrace writeOutput $
        do  ops <- first show <$> Parse.ops =<< rawOps
            genop <- first show <$> Parse.genop =<< rawGenop
            pure $ Code.generate ops genop


getVersion :: [String] -> String
getVersion args =
  let context = " erlang version/branch command-line argument!" in
  case args of
    [version]  -> version
    []         -> errorWithoutStackTrace $ "Missing" ++ context
    _          -> errorWithoutStackTrace $ "Ambigious" ++ context


download :: String -> String -> IO (Either String String)
download version path =
  openURIString (rootUrl ++ version ++ path)
  where
    rootUrl = "https://raw.githubusercontent.com/erlang/otp/"


writeOutput :: Haskell.Module () -> IO ()
writeOutput =
  writeFile "src/Codec/Beam/Generated.hs" . Haskell.prettyPrint
