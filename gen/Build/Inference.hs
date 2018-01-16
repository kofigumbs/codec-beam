module Build.Inference (run) where

import qualified Data.Set as Set

import Types


run :: [Line] -> [OpCode] -> [Definition]
run _lines _opCodes =
  [ Definition "move" [Set.fromList [Atom, Literal], Set.singleton XRegister]
  ]
