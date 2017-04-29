module Codec.Beam
  ( Module(..), empty
  , put
  ) where

import Data.ByteString (ByteString)
import Data.Binary.Put


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


empty :: ByteString -> Module
empty name =
  Module
    { _name = name
    , _atoms = []
    , _code = ()
    , _imports = []
    , _exports = []
    }



-- SERIALIZE


put :: Module -> Put
put _modul =
  do  return ()
