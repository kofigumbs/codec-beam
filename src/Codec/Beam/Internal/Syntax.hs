module Codec.Beam.Internal.Syntax where

import Data.ByteString.Lazy (ByteString)
import Data.Void (Void)
import Data.Word (Word8)
import Unsafe.Coerce (unsafeCoerce)


-- | A virtual machine instruction—the main unit this library deals with.
--   There are a finite number of instructions, enumerated in "Codec.Beam.Instructions".
--   Each new release of Erlang/OTP might introduce a few more and deprecate old ones.
data Op = Op Word8 [Argument]


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

-- | Convert BIF to a normal import with zero arguments,
--   whichcan be used with 'Codec.Beam.Instructions.call' and friends.
importBif0 :: Bif0 a => a -> Import
importBif0 = bif_ 0

-- | Convert BIF to a normal import with one argument.
importBif1 :: Bif1 a => a -> Import
importBif1 = bif_ 1

-- | Convert BIF to a normal import with two arguments.
importBif2 :: Bif2 a => a -> Import
importBif2 = bif_ 2

-- | Convert BIF to a normal import with three arguments.
importBif3 :: Bif3 a => a -> Import
importBif3 = bif_ 3

-- | Convert BIF to a normal import with four arguments.
importBif4 :: Bif4 a => a -> Import
importBif4 = bif_ 4

class Bif_ a => Bif0 a
class Bif_ a => Bif1 a
class Bif_ a => Bif2 a
class Bif_ a => Bif3 a
class Bif_ a => Bif4 a


-- | Either type of stack register, 'X' or 'Y'.
--   Instructions that work with this type, use 'IsRegister' for convenience.
newtype  Register  = Register { unRegister :: Argument }
class    IsRegister a        where toRegister :: a -> Register
instance IsRegister Register where toRegister = id               ;{-# INLINE toRegister #-}
instance IsRegister X        where toRegister = Register . FromX ;{-# INLINE toRegister #-}
instance IsRegister Y        where toRegister = Register . FromY ;{-# INLINE toRegister #-}


-- | Any sort of Erlang value.
--   Instructions that work with this type, use 'IsSource' for convenience.
newtype  Source = Source { unSource :: Argument }
class    IsSource a          where toSource :: a -> Source
instance IsSource Source     where toSource = id                      ;{-# INLINE toSource #-}
instance IsSource X          where toSource = Source . FromX          ;{-# INLINE toSource #-}
instance IsSource Y          where toSource = Source . FromY          ;{-# INLINE toSource #-}
instance IsSource Nil        where toSource = Source . FromNil        ;{-# INLINE toSource #-}
instance IsSource ByteString where toSource = Source . FromByteString ;{-# INLINE toSource #-}
instance IsSource Literal    where toSource = Source . FromLiteral    ;{-# INLINE toSource #-}
instance IsSource Int        where toSource = Source . FromInt        ;{-# INLINE toSource #-}


-- | Memory for manipulating 'F', for use with 'Codec.Beam.Instructions.fmove'.
--   Instructions that work with this type, use 'IsRegisterF' for convenience.
newtype  RegisterF = RegisterF { unRegisterF :: Argument }
class    IsRegisterF a         where toRegisterF :: a -> RegisterF
instance IsRegisterF RegisterF where toRegisterF = id                ;{-# INLINE toRegisterF #-}
instance IsRegisterF F         where toRegisterF = RegisterF . FromF ;{-# INLINE toRegisterF #-}
instance IsRegisterF X         where toRegisterF = RegisterF . FromX ;{-# INLINE toRegisterF #-}
instance IsRegisterF Y         where toRegisterF = RegisterF . FromY ;{-# INLINE toRegisterF #-}


-- | Something that can be coerced into 'F', for use with 'Codec.Beam.Instructions.fmove'.
--   Instructions that work with this type, use 'IsSourceF' for convenience.
newtype  SourceF = SourceF { unSourceF :: Argument }
class    IsSourceF a          where toSourceF :: a -> SourceF
instance IsSourceF SourceF    where toSourceF = id                       ;{-# INLINE toSourceF #-}
instance IsSourceF F          where toSourceF = SourceF . FromF          ;{-# INLINE toSourceF #-}
instance IsSourceF X          where toSourceF = SourceF . FromX          ;{-# INLINE toSourceF #-}
instance IsSourceF Y          where toSourceF = SourceF . FromY          ;{-# INLINE toSourceF #-}
instance IsSourceF Literal    where toSourceF = SourceF . FromLiteral    ;{-# INLINE toSourceF #-}



-- PRIVATE, not exposed outside of package


data Argument
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
  | FromList [Argument]
  | FromFunctionModule ByteString Int


fromRegister :: IsRegister a => a -> Argument
{-# INLINE fromRegister #-}
fromRegister = unRegister . toRegister


fromSource :: IsSource a => a -> Argument
{-# INLINE fromSource #-}
fromSource = unSource . toSource


fromRegisterF :: IsRegisterF a => a -> Argument
{-# INLINE fromRegisterF #-}
fromRegisterF = unRegisterF . toRegisterF


fromSourceF :: IsSourceF a => a -> Argument
{-# INLINE fromSourceF #-}
fromSourceF = unSourceF . toSourceF


fromPairs :: [(Source, Source)] -> Argument
{-# INLINE fromPairs #-}
fromPairs =
  FromList . concatMap (\x -> [fromSource (fst x), fromSource (snd x)])


fromDestinations :: [(Label, Source)] -> Argument
{-# INLINE fromDestinations #-}
fromDestinations =
  FromList . concatMap (\x -> [FromLabel (fst x), fromSource (snd x)])


class Bif_ a where bif_ :: Int -> a -> Import
