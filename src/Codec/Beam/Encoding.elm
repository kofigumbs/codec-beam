module Codec.Beam.Encoding exposing (for)

import Bytes exposing ((<>), Bytes)
import Dict exposing (Dict)


for : Dict String Int -> Bytes
for atomTable =
    let
        sections =
            Bytes.concat
                [ Bytes.string "Atom" <> alignSection (atoms atomTable)
                , Bytes.string "LocT" <> alignSection (Bytes.int32 0)
                , Bytes.string "StrT" <> alignSection (Bytes.int32 0)
                , Bytes.string "ImpT" <> alignSection (Bytes.int32 0)
                , Bytes.string "ExpT" <> alignSection (Bytes.int32 0)
                , Bytes.string "Code" <> alignSection (code "" 0 0)
                ]
    in
    Bytes.concat
        [ Bytes.string "FOR1" <> Bytes.int32 (Bytes.length sections + 4)
        , Bytes.string "BEAM" <> sections
        ]


atoms : Dict String Int -> Bytes
atoms table =
    let
        encode ( name, _ ) =
            Bytes.int8 (String.length name) <> Bytes.string name

        names =
            List.sortBy Tuple.second (Dict.toList table)
    in
    Bytes.int32 (List.length names) <> Bytes.concat (List.map encode names)


code : String -> Int -> Int -> Bytes
code builder labelCount functionCount =
    let
        headerLength =
            16

        instructionSetId =
            0

        maxOpCode =
            158

        intCodeEnd =
            3
    in
    Bytes.concat
        [ Bytes.int32 headerLength
        , Bytes.int32 instructionSetId
        , Bytes.int32 maxOpCode
        , Bytes.int32 labelCount
        , Bytes.int32 functionCount
        , Bytes.string builder
        , Bytes.int8 intCodeEnd
        ]


alignSection : Bytes -> Bytes
alignSection bytes =
    let
        size =
            Bytes.length bytes
    in
    Bytes.int32 size <> bytes <> Bytes.pad 4 size
