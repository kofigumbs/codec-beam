module Codec.Beam.Internal.Types where

import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8)


-- | You can find implementations in "Codec.Beam.Instructions"
data Op = Op Word8 [Argument ()]


-- | A stack register! These are used to pass function arguments, and @X 0@ stores return values.
newtype X = X Int
  deriving (Eq, Ord, Show)


-- | A stack register for saving values across function calls.
--   Anything you put in a 'X' register can be overwritten inside a function call
--   (or inside a function call inside a function call).
--   @Y@ registers let you avoid that—they must be allocated and de-allocated though.
newtype Y = Y Int
  deriving (Eq, Ord, Show)


-- | Floating point \"register\" for optimized floating point arithmetic.
--   These are not treated as traditional stack registers.
newtype F = F Int
  deriving (Eq, Ord, Show)


-- | Reference a function from another module
--   For example, @Import "erlang" "+" 2@ refers to the stdlib function: @erlang:'+'/2@ .
data Import = Import
  { _import_module :: ByteString
  , _import_function :: ByteString
  , _import_arity :: Int
  }
  deriving (Eq, Ord, Show)


-- | Turn a named function into a @fun@, for use with 'Codec.Beam.Instructions.make_fun2'.
data Lambda = Lambda
  { _lambda_name :: ByteString
  , _lambda_arity :: Int
  , _lambda_label :: Label
  , _lambda_free :: Int
  }
  deriving (Eq, Ord, Show)


-- | Mark a spot in the code, so that you can jump to it with a function or condition.
newtype Label = Label Int
  deriving (Eq, Ord, Show)


-- | The empty list.
data Nil = Nil
  deriving (Eq, Ord, Show)


-- | Erlang literals, stored on the heap.
data Literal
  = Integer Int
  | Float Double
  | Atom ByteString
  | Binary ByteString
  | Tuple [Literal]
  | List [Literal]
  | Map [(Literal, Literal)]
  deriving (Eq, Ord, Show)


{- TODO
  | String ByteString
  | Port ...
  | Pid ProcessId
  | Fun ProcessId ModuleName Lambda [Literal]

data ModuleName
  = This
  | External ByteString

data ProcessId
  = ProcessId ...
-}


-- | Create jump destinations for variadic functions, like 'Codec.Beam.Instructions.select_val'
--   Use 'destination' to make values of this type.
data Destination = Destination { _destination_args :: [Argument ()] }
destination :: (Source s) => Label -> s -> Destination
destination label source = Destination [FromLabel label, erase fromSource source]


-- | Create map pairs for variadic functions, like 'Codec.Beam.Instructions.put_map_assoc'
--   Use 'pair' to make values of this type.
data Pair = Pair { _pair_args :: [Argument ()] }
pair :: (Source key, Source value) => key -> value -> Pair
pair key value = Pair [erase fromSource key, erase fromSource value]


-- | Create map fields for variadic functions, like 'Codec.Beam.Instructions.has_map_fields'
--   Use 'field' to make values of this type.
newtype Field = Field { _field_arg :: Argument () }
field :: (Source s) => s -> Field
field source = Field (erase fromSource source)


class    Register a where fromRegister :: a -> Argument Register_
instance Register X where fromRegister = FromX ;{-# INLINE fromRegister #-}
instance Register Y where fromRegister = FromY ;{-# INLINE fromRegister #-}


class    RegisterF a where fromRegisterF :: a -> Argument RegisterF_
instance RegisterF F where fromRegisterF = FromF ;{-# INLINE fromRegisterF #-}
instance RegisterF X where fromRegisterF = FromX ;{-# INLINE fromRegisterF #-}
instance RegisterF Y where fromRegisterF = FromY ;{-# INLINE fromRegisterF #-}


class    Source a          where fromSource :: a -> Argument Source_
instance Source X          where fromSource = FromX          ;{-# INLINE fromSource #-}
instance Source Y          where fromSource = FromY          ;{-# INLINE fromSource #-}
instance Source Nil        where fromSource = FromNil        ;{-# INLINE fromSource #-}
instance Source ByteString where fromSource = FromByteString ;{-# INLINE fromSource #-}
instance Source Literal    where fromSource = FromLiteral    ;{-# INLINE fromSource #-}
instance Source Int        where fromSource = FromInt        ;{-# INLINE fromSource #-}


class    SourceF a          where fromSourceF :: a -> Argument SourceF_
instance SourceF F          where fromSourceF = FromF          ;{-# INLINE fromSourceF #-}
instance SourceF X          where fromSourceF = FromX          ;{-# INLINE fromSourceF #-}
instance SourceF Y          where fromSourceF = FromY          ;{-# INLINE fromSourceF #-}
instance SourceF Literal    where fromSourceF = FromLiteral    ;{-# INLINE fromSourceF #-}



-- PRIVATE, not exposed outside of package


data Argument a
  = FromImport Import
  | FromX X
  | FromY Y
  | FromF F
  | FromNewLabel Label
  | FromUntagged Int
  | FromInt Int
  | FromNil Nil
  | FromByteString ByteString
  | FromLabel Label
  | FromLiteral Literal
  | FromLambda Lambda
  | FromDestinations [Destination]
  | FromPairs [Pair]
  | FromFields [Field]
  | FromFunctionModule ByteString Int


erase :: (a -> Argument b) -> a -> Argument c
erase f a =
  case f a of
    FromImport x           -> FromImport x
    FromX x                -> FromX x
    FromY x                -> FromY x
    FromF x                -> FromF x
    FromNewLabel x         -> FromNewLabel x
    FromUntagged x         -> FromUntagged x
    FromInt x              -> FromInt x
    FromNil x              -> FromNil x
    FromByteString x       -> FromByteString x
    FromLabel x            -> FromLabel x
    FromLiteral x          -> FromLiteral x
    FromLambda x           -> FromLambda x
    FromDestinations x     -> FromDestinations x
    FromPairs x            -> FromPairs x
    FromFields x           -> FromFields x
    FromFunctionModule f a -> FromFunctionModule f a


-- Phantom "Argument" types
-- This lets us prevent mixing-and-matching outside the package,
-- while still allowing users to express their own types in terms of argument constraints.
data Register_  = Register_
data RegisterF_ = RegisterF_
data Source_    = Source_
data SourceF_   = SourceF_
