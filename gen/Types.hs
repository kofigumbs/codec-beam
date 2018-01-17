module Types where

import Data.Set (Set)


data OpCode = OpCode
  { _o_deprecated :: Bool
  , _o_code :: Int
  , _o_name :: String
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
  = NameOnly String
  | TypeOnly [Type]
  | Complete String [Type]
  deriving Show


data Type
  = Import
  | Atom
  | XRegister
  | YRegister
  | FloatRegister
  | Literal
  | Label
  | VarArgs
  | Untagged
  deriving (Eq, Ord, Show)


data Definition = Definition
  { _d_name :: String
  , _d_code :: Int
  , _d_args :: [Set Type]
  }
