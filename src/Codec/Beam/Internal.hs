module Codec.Beam.Internal where

import Control.Monad.State.Strict (State)
import Data.ByteString.Lazy (ByteString)
import Data.Table (Table)
import Data.Word (Word8, Word32)
import qualified Data.ByteString.Builder as BS


-- | You can find implementations in "Codec.Beam.Genop"
data Op
  = Op Word8 (State Builder [Operand])


data Operand
  = Lit Int
  | Int Int
  | Nil
  | Atom ByteString
  | Reg Register
  | Label Label
  | Ext Literal


data Literal
  = EInt Int
  | EFloat Double
  | EAtom ByteString
  | EBinary ByteString
  | ETuple [Literal]
  | EList [Literal]
  | EMap [(Literal, Literal)]
  deriving (Eq, Ord)


data Lambda
  = Lambda
      { _l_name :: ByteString
      , _l_arity :: Int
      , _l_label :: Int
      , _l_index :: Int
      , _l_free :: Int
      }


data Register
  = X Int
  | Y Int


type Label
  = Int


type Export
  = (ByteString, Int, Int)


data Access
  = Public
  | Private


data Function
  = Function
      { _f_module :: ByteString
      , _f_name :: ByteString
      , _f_arity :: Int
      }
  deriving (Eq, Ord)


data Builder =
  Builder
    { _moduleName :: Operand
    , _overallLabelCount :: Int
    , _currentLabelCount :: Int
    , _functionCount :: Word32
    , _atomTable :: Table ByteString
    , _literalTable :: Table Literal
    , _lambdaTable :: [Lambda]
    , _importTable :: Table Function
    , _exportNextLabel :: Maybe (ByteString, Int)
    , _toExport :: [Export]
    , _code :: BS.Builder
    }
