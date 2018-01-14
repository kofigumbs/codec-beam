module Code (generate) where

import qualified Ops
import Language.Haskell.Exts.Syntax (Module)

generate :: [Ops.Line] -> [Ops.OpCode] -> Module ()
generate =
  error "TODO"
