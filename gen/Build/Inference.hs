module Build.Inference (run) where

import qualified Data.Set as Set

import Types


run :: [Line] -> [OpCode] -> [Definition]
run _lines _opCodes =
  [ Definition "move" 64
      [ Set.fromList [YRegister, FloatRegister]
      , Set.singleton XRegister
      , Set.fromList [Atom, Literal]
      ]
  ]
