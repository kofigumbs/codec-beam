module Codec.Beam.Genop.Internal (addImport) where

import Data.ByteString.Lazy (ByteString)
import qualified Control.Monad.State.Strict as State
import qualified Data.Table as Table

import Codec.Beam.Internal


addImport :: ByteString -> ByteString -> Int -> State.State Builder [Operand]
addImport module_ function arity =
  do builder <- State.get
     let (index, newTable) =
          Table.index (Function module_ function arity) (_importTable builder)
     State.put $ builder
       { _importTable = newTable
       , _atomTable = insert module_ $ insert function $ _atomTable builder
       }
     return [ Lit arity, Lit index ]


insert :: Ord k => k -> Table.Table k -> Table.Table k
insert key =
  snd . Table.index key
