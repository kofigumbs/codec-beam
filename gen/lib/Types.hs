module Types where


data OpCode = OpCode
  { _op_code :: Int
  , _op_name :: String
  }


data Line
  = GenericOp String
  | SpecificOp String [[Type]]
  | Transform [Instruction] [Instruction]


data Instruction
  = C
  | Op String [Argument]


data Argument = Argument
  { _arg_name :: Maybe String
  , _arg_type :: [Type]
  }


data Type
  = Import
  | Atom
  | XRegister
  | YRegister
  | FloatRegister
  | Literal
  | Label
  | Untagged
  deriving (Eq, Ord, Show)


data Definition = Definition
  { _def_name :: String
  , _def_code :: Int
  , _def_args :: [[Type]]
  }
