module Codec.Beam.Internal.Syntax where

import Data.ByteString.Lazy (ByteString)
import Data.Void (Void)
import Data.Word (Word8)
import Unsafe.Coerce (unsafeCoerce)


-- | A virtual machine instruction—the main unit this library deals with.
--   There are a finite number of instructions, enumerated in "Codec.Beam.Instructions".
--   Each new release of Erlang/OTP might introduce a few more and deprecate old ones.
data Op = Op Word8 [Argument ()]


-- | Mark a spot in the code, so that you can jump to it with a function or condition.
newtype Label = Label Int
  deriving (Eq, Ord, Show)


-- | A stack register. These are used to pass function arguments, and @X 0@ stores return values.
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


-- | The empty list.
data Nil = Nil
  deriving (Eq, Ord, Show)


-- | Turn a named function into a @fun@, for use with 'Codec.Beam.Instructions.make_fun2'.
data Lambda = Lambda
  { _lambda_name :: ByteString -- ^ unique name for this lambda
  , _lambda_arity :: Int
  , _lambda_label :: Label     -- ^ where to find the backing functino
  , _lambda_free :: Int        -- ^ how many variables to capture from calling scope
  }
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


-- | Reference a function from another module.
--   For example, @Import "array" "map" 2@ refers to the stdlib function: @array:map/2@.
data Import = Import
  { _import_module :: ByteString
  , _import_function :: ByteString
  , _import_arity :: Int
  }
  deriving (Eq, Ord, Show)


-- | This constraint marks functions that do not require heap storage,
--   which means they can be called without concern for garbage collection.
class NoGC a

class Bif_ a => Bif0 a
class Bif_ a => Bif1 a
class Bif_ a => Bif2 a
class Bif_ a => Bif3 a
class Bif_ a => Bif4 a

-- | Convert BIF to a normal import with zero arguments,
--   whichcan be used with 'Codec.Beam.Instructions.call' and friends.
importBif0 :: Bif0 a => a -> Import
importBif0 = importBif_ 0

-- | Convert BIF to a normal import with one argument.
importBif1 :: Bif1 a => a -> Import
importBif1 = importBif_ 1

-- | Convert BIF to a normal import with two arguments.
importBif2 :: Bif2 a => a -> Import
importBif2 = importBif_ 2

-- | Convert BIF to a normal import with three arguments.
importBif3 :: Bif3 a => a -> Import
importBif3 = importBif_ 3

-- | Convert BIF to a normal import with four arguments.
importBif4 :: Bif4 a => a -> Import
importBif4 = importBif_ 4

importBif_ :: Bif_ a => Int -> a -> Import
importBif_ arity a = Import module_ function arity
  where (module_, function) = bif_ a


-- | Create jump destinations for variadic functions, like 'Codec.Beam.Instructions.select_val'.
--   Use 'destination' to make values of this type.
data Destination = Destination { _destination_args :: [Argument ()] }
destination :: (Source s) => Label -> s -> Destination
destination label source = Destination [FromLabel label, erase fromSource source]


-- | Create map pairs for variadic functions, like 'Codec.Beam.Instructions.put_map_assoc'.
--   Use 'pair' to make values of this type.
data Pair = Pair { _pair_args :: [Argument ()] }
pair :: (Source key, Source value) => key -> value -> Pair
pair key value = Pair [erase fromSource key, erase fromSource value]


-- | Create map fields for variadic functions, like 'Codec.Beam.Instructions.has_map_fields'.
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


-- Drops the phantom type, so that we can use it inside 'Op's
erase :: (a -> Argument b) -> a -> Argument ()
erase f a =
  unsafeCoerce (f a)


-- Phantom 'Argument' types
-- This lets us prevent mixing-and-matching outside the package,
-- while still allowing users to express their own types in terms of argument constraints.
newtype Register_  = Register_  Register_
newtype RegisterF_ = RegisterF_ RegisterF_
newtype Source_    = Source_    Source_
newtype SourceF_   = SourceF_   SourceF_


class Bif_ a where bif_ :: a -> (ByteString, ByteString)
