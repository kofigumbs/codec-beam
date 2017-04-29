module Codec.Beam
  ( Module(..), empty
  , put
  ) where

import Data.ByteString (ByteString)
import Data.Binary.Put


-- AST


data Module
  = Module
      { _name :: ByteString
      , _atoms :: [ByteString]
      , _code :: Code
      , _imports :: [(ByteString, ByteString, Int)]
      , _exports :: [(ByteString, Int)]
      }


type Code
  = ()



-- SERIALIZE


put :: Module -> Put
put _modul =
  do  return ()



-- HELPERS


empty :: ByteString -> Module
empty name =
  Module
    { _name = name
    , _atoms = []
    , _code = ()
    , _imports = []
    , _exports = []
    }
