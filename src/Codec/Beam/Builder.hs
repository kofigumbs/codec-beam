module Codec.Beam.Builder where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Word (Word8, Word32)


{-| Create structurally correct BEAM code.
 -}


data Op
  = Op Word8 (Builder -> ([Operand], Builder))


data Operand
  = Lit Int
  | Int Int
  | Nil
  | Atom BS.ByteString
  | Reg Register
  | Label Label
  | Ext Literal


data Literal
  = EInt Int
  | EFloat Double
  | EAtom BS.ByteString
  | EBinary BS.ByteString
  | ETuple [Literal]
  | EList [Literal]


data Lambda
  = Lambda
      { _name :: BS.ByteString
      , _arity :: Int
      , _label :: Int
      , _index :: Int
      , _free :: Int
      }


data Register
  = X Int
  | Y Int


type Label
  = Int


type Export
  = (BS.ByteString, Int, Int)


data Access
  = Public
  | Private


data Builder =
  Builder
    { _moduleName :: Operand
    , _overallLabelCount :: Int
    , _currentLabelCount :: Int
    , _functionCount :: Word32
    , _atomTable :: Map.Map BS.ByteString Int
    , _literalTable :: [Literal]
    , _lambdaTable :: [Lambda]
    , _exportNextLabel :: Maybe (BS.ByteString, Int)
    , _toExport :: [Export]
    , _code :: Builder.Builder
    }
