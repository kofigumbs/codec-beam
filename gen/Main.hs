import System.Process (readProcess)
import System.Environment (getArgs)

import qualified Ops

main :: IO ()
main =
  do  commandLineArguments <- getArgs
      case commandLineArguments of
        [] ->
          error "Missing erlang version command-line argument!"
        v : _ ->
          print =<< Ops.parse <$> readProcess "curl" [ Ops.downloadUrl v ] ""
