module Build.Inference (run) where

import Types


run :: [Line] -> [OpCode] -> [Definition]
run _lines _opCodes =
  [ Definition "move" 64
      [ [YRegister, FloatRegister]
      , [XRegister]
      , [Atom, Literal]
      ]
  ]
