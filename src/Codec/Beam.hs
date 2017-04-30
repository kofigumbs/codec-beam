module Codec.Beam
  ( Module(..), empty
  , encode
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Data.Bits ((.&.))


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
    chunk id body =
      id <> packLength body <> body

    sections =
      BS.concat
        [ chunk "Atom" (atoms (_name beam : _atoms beam))
        , chunk "Code" (code (_code beam))
        , chunk "StrT" (strings (_strings beam))
        , chunk "ImpT" (imports (_imports beam))
        , chunk "ExpT" (exports (_exports beam))
        ]
  in
    "FOR1" <> packLength sections <> "BEAM" <> sections


atoms :: [ByteString] -> ByteString
atoms _ =
  ""

code :: [Statement] -> ByteString
code _ =
  ""


strings :: [ByteString] -> ByteString
strings _ =
  ""


imports :: [(ByteString, ByteString, Int)] -> ByteString
imports _ =
  ""

exports :: [(ByteString, Int)] -> ByteString
exports _ =
  ""



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


packLength :: ByteString -> ByteString
packLength string =
  let
    word offset =
      fromIntegral (offset .&. BS.length string)
  in
    BS.pack
      [ word 0o7000
      , word 0o0700
      , word 0o0070
      , word 0o0007
      ]
