import qualified Ops
import System.Process


main :: IO ()
main =
  print =<< Ops.parse <$> readProcess "curl" [ Ops.downloadUrl "maint-20" ] ""
