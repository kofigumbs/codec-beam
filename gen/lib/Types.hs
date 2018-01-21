module Types where


data OpCode = OpCode
  { _op_deprecated :: Bool
  , _op_code :: Int
  , _op_name :: String
  }
  deriving Show


data Line
  = Transform [Instruction] [Instruction]
  | SpecificOp String [[Type]]
  | GenericOp String
  deriving (Eq, Ord, Show)


data Instruction
  = C
  | Op String [Argument]
  deriving (Eq, Ord, Show)


data Argument = Argument
  { _arg_name :: Maybe String
  , _arg_type :: [Type]
  }
  deriving (Eq, Ord, Show)


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
