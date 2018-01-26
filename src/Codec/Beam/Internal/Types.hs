module Codec.Beam.Internal.Types where

import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8)


-- | You can find implementations in "Codec.Beam.Instruction"
data Op = Op Word8 [Argument]


-- | A stack register! These are used to pass function arguments,
--   and @X 0@ stores return values.
newtype X = X Int


-- | A stack register for saving values across function calls.
--   Anything you put in a 'X' register can be overwritten inside a function call
--   (or inside a function call inside a function call).
--   @Y@ registers let you avoid that—they must be allocated and de-allocated though.
newtype Y = Y Int


-- | Floating point "register" for optimized floating point arithmetic.
--   These are not treated as traditional stack registers.
newtype F = F Int


-- | Reference a function from another module
-- | For example, @Import "erlang" "+" 2@ refers to the stdlib function: @erlang:'+'/2@ .
data Import = Import
  { _module :: ByteString
  , _function :: ByteString
  , _arity :: Int
  }


-- | Mark a spot in the code, so that you can jump to it with a function or condition.
newtype Label = Label Int


-- | Erlang literals, stored on the heap.
data Literal
  = Integer Int
  | Float Double
  | Atom ByteString
  | Binary ByteString
  | Tuple [Literal]
  | List [Literal]
  | Map [(Literal, Literal)]


class    Register a where fromRegister :: a -> Argument
instance Register X where fromRegister = FromX ;{-# INLINE fromRegister #-}
instance Register Y where fromRegister = FromY ;{-# INLINE fromRegister #-}


class    RegisterF a where fromRegisterF :: a -> Argument
instance RegisterF F where fromRegisterF = FromF        ;{-# INLINE fromRegisterF #-}
instance RegisterF X where fromRegisterF = FromX        ;{-# INLINE fromRegisterF #-}
instance RegisterF Y where fromRegisterF = FromY        ;{-# INLINE fromRegisterF #-}


class    Source a          where fromSource :: a -> Argument
instance Source X          where fromSource = FromX          ;{-# INLINE fromSource #-}
instance Source Y          where fromSource = FromY          ;{-# INLINE fromSource #-}
instance Source ByteString where fromSource = FromByteString ;{-# INLINE fromSource #-}
instance Source Literal    where fromSource = FromLiteral    ;{-# INLINE fromSource #-}
instance Source Int        where fromSource = FromInt        ;{-# INLINE fromSource #-}


class    SourceF a          where fromSourceF :: a -> Argument
instance SourceF F          where fromSourceF = FromF          ;{-# INLINE fromSourceF #-}
instance SourceF X          where fromSourceF = FromX          ;{-# INLINE fromSourceF #-}
instance SourceF Y          where fromSourceF = FromY          ;{-# INLINE fromSourceF #-}
instance SourceF ByteString where fromSourceF = FromByteString ;{-# INLINE fromSourceF #-}
instance SourceF Literal    where fromSourceF = FromLiteral    ;{-# INLINE fromSourceF #-}
instance SourceF Int        where fromSourceF = FromInt        ;{-# INLINE fromSourceF #-}



-- PRIVATE


-- Makes it safer to use bracket pattern for ops with var-args
newtype Variadic a = Variadic { _args :: [a] }


data Argument
  = FromImport Import
  | FromX X
  | FromY Y
  | FromF F
  | FromInt Int
  | FromByteString ByteString
  | FromLabel Label
  | FromLiteral Literal
  | FromList [Argument]
  | MODULE_NAME
