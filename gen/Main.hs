import System.Process (readProcess)
import System.Environment (getArgs)

import qualified Code
import qualified Parse


main :: IO ()
main =
  getArgs >>= \commandLineArguments ->
    case commandLineArguments of
      [] ->
        error "Missing erlang version command-line argument!"
      version : _ ->
        do  opsTab <- download version "/erts/emulator/beam/ops.tab"
            genopTab <- download version "/lib/compiler/src/genop.tab"
            case Code.generate <$> Parse.ops opsTab <*> Parse.genop genopTab of
              Left parseError -> error $ show parseError
              Right haskell -> print haskell -- TODO: write to src/


download :: String -> String -> IO String
download version path =
  readProcess "curl" [ rootUrl ++ version ++ path ] ""
  where
    rootUrl = "https://raw.githubusercontent.com/erlang/otp/"
