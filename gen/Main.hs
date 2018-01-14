import System.Process (readProcess)
import System.Environment (getArgs)

import qualified Ops

main :: IO ()
main =
  getArgs >>= \commandLineArguments ->
    case commandLineArguments of
      [] ->
        error "Missing erlang version command-line argument!"
      v : _ ->
        do  ops <- Ops.parse <$> readProcess "curl" [ Ops.downloadUrl v ] ""
            print ops
