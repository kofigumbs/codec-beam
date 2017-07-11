module Codec.Beam exposing (Builder, append, encode, new, toBytes)

import Bytes exposing (Bytes)
import Codec.Beam.Encoding as Encoding
import Dict exposing (Dict)


type Op
    = Label Int


type Builder
    = Builder
        { atomTable : Dict String Int
        , code : Bytes
        }


new : String -> Builder
new name =
    Builder
        { atomTable = Dict.singleton name 1
        , code = Bytes.empty
        }


encode : String -> List Op -> Bytes
encode name ops =
    new name |> append ops |> toBytes


append : List Op -> Builder -> Builder
append _ builder =
    -- TODO
    builder


toBytes : Builder -> Bytes
toBytes (Builder { atomTable }) =
    Encoding.for atomTable
