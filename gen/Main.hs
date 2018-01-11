import qualified Ops
import System.Process


version :: String
version =
  "maint-20"


downloadUrl :: String
downloadUrl =
  "https://raw.githubusercontent.com/erlang/otp/"
    ++ version
    ++ "/erts/emulator/beam/ops.tab"


main :: IO ()
main =
  do  raw <- readProcess "curl" [downloadUrl] ""
      print $ Ops.parse raw
