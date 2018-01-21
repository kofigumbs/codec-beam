module Codec.Beam.Internal.Types where

import Data.ByteString.Lazy (ByteString)
import Data.Word (Word8)


-- | You can find implementations in "Codec.Beam.Genop"
data Op = Op Word8 [Encoding]


-- | Reference a function from another module
-- | For example, @Import "erlang" "+" 2@ refers to the stdlib function: @erlang:'+'/2@ .
data Import = Import
  { _module :: ByteString
  , _function :: ByteString
  , _arity :: Int
  }


-- | TODO: add docs
newtype X = X Int
newtype Y = Y Int
newtype F = F Int
newtype Label = Label Int


data Literal
  = Integer Int
  | Float Double
  | Atom ByteString
  | Binary ByteString
  | Tuple [Literal]
  | List [Literal]
  | Map [(Literal, Literal)]


data Encoding
  = FromImport Import
  | FromX X
  | FromY Y
  | FromF F
  | FromInteger Int
  | FromByteString ByteString
  | FromLabel Label
  | FromLiteral Literal
