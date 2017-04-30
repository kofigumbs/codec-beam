module Codec.Beam
  ( Module(..), empty
  , encode
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Binary.Put


-- AST


data Module
  = Module
      { _name :: ByteString
      , _atoms :: [ByteString]
      , _code :: [Statement]
      , _strings :: [ByteString]
      , _imports :: [(ByteString, ByteString, Int)]
      , _exports :: [(ByteString, Int)]
      }


data Statement
  = TODO



-- SERIALIZE


encode :: Module -> ByteString
encode beam =
  let
    sections =
      BS.concat
        [ makeChunk "Atom" (atoms (_name beam : _atoms beam))
        , makeChunk "Code" (code (_code beam))
        , makeChunk "StrT" (strings (_strings beam))
        , makeChunk "ImpT" (imports (_imports beam))
        , makeChunk "ExpT" (exports (_exports beam))
        ]
  in
    runPut $
      do  putByteString "FOR1"
          putWord8 (fromIntegral (BS.length sections))
          putByteString "BEAM"
          putLazyByteString sections


makeChunk :: ByteString -> Put -> ByteString
makeChunk _id _body =
  ""

atoms :: [ByteString] -> Put
atoms _ =
  return ()

code :: [Statement] -> Put
code _ =
  return ()


strings :: [ByteString] -> Put
strings _ =
  return ()


imports :: [(ByteString, ByteString, Int)] -> Put
imports _ =
  return ()

exports :: [(ByteString, Int)] -> Put
exports _ =
  return ()



-- HELPERS


empty :: ByteString -> Module
empty name =
  Module
    { _name = name
    , _atoms = []
    , _code = []
    , _strings = []
    , _imports = []
    , _exports = []
    }
