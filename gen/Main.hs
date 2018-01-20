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
        do  ops <- first show <$> Build.Parse.ops =<< rawOps
            genop <- first show <$> Build.Parse.genop =<< rawGenop
            pure $ writeFile "src/Codec/Beam/Internal/Generated.hs" $
              Build.Generate.code "Codec.Beam.Internal.Generated" $
              Build.Inference.run ops genop


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
