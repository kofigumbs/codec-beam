module Ops where

-- TYPES ONLY

data OpCode = OpCode
  { _deprecated :: Bool
  , _code :: Int
  , _name :: String
  }
  deriving Show

data Line
  = GenericOp String
  | SpecificOp String [Type]
  | Transform [Instruction] [Instruction]
  deriving Show

data Instruction
  = C
  | Op String [Argument]
  deriving Show

data Argument
  = NameOnly String
  | TypeOnly Type
  | Complete String Type
  deriving Show

data Type
  = Import
  | Export
  | Atom
  | XRegister
  | YRegister
  | FloatRegister
  | Untagged
  | Literal
  | Label
  | VarArgs
  | Union [Type]
  deriving (Eq, Ord, Show)
