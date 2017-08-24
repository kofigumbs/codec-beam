module Codec.Beam
  ( encode
  , Op, Operand(..), Register(..), Access(..), Literal(..)
  , Builder, new, append, toLazyByteString
  , module Codec.Beam.Genop
  ) where


import Codec.Beam.Builder
import Codec.Beam.Genop
