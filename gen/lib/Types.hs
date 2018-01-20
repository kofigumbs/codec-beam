module Types where


data OpCode = OpCode
  { _op_deprecated :: Bool
  , _op_code :: Int
  , _op_name :: String
  }
  deriving Show


data Line
  = GenericOp String
  | SpecificOp String [[Type]]
  | Transform [Instruction] [Instruction]
  deriving Show


data Instruction
  = C
  | Op String [Argument]
  deriving Show


data Argument
  = Argument (Maybe String) [Type]
  deriving Show


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
