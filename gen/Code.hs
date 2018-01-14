module Code (generate) where

import Types
import Language.Haskell.Exts.Syntax (Module)

generate :: [Line] -> [OpCode] -> Module ()
generate =
  error "TODO"
