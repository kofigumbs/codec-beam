module Codec.Beam exposing (..)

import Bitwise
import Char
import Dict exposing (Dict)


type alias ByteArray =
    List Int


for : Dict String Int -> ByteArray
for atomTable =
    let
        sections =
            List.concat
                [ string "Atom" ++ alignSection (atoms atomTable)
                , string "LocT" ++ alignSection (int32 0)
                , string "StrT" ++ alignSection (int32 0)
                , string "ImpT" ++ alignSection (int32 0)
                , string "ExpT" ++ alignSection (int32 0)
                , string "Code" ++ alignSection (code "" 0 0)
                ]
    in
    List.concat
        [ string "FOR1" ++ int32 (List.length sections + 4)
        , string "BEAM" ++ sections
        ]


atoms : Dict String Int -> ByteArray
atoms table =
    let
        encode ( name, _ ) =
            word (String.length name) :: string name

        names =
            List.sortBy Tuple.second (Dict.toList table)
    in
    int32 (List.length names) ++ List.concat (List.map encode names)


code : String -> Int -> Int -> ByteArray
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
    List.concat
        [ int32 headerLength
        , int32 instructionSetId
        , int32 maxOpCode
        , int32 labelCount
        , int32 functionCount
        , string builder
        , int8 intCodeEnd
        ]


alignSection : ByteArray -> ByteArray
alignSection bytes =
    let
        size =
            List.length bytes

        padding =
            case size % 4 of
                0 ->
                    []

                r ->
                    List.repeat (4 - r) 0
    in
    int32 size ++ bytes ++ padding


string : String -> ByteArray
string =
    String.toList >> List.map Char.toCode


int8 : Int -> ByteArray
int8 =
    word >> List.singleton


int32 : Int -> ByteArray
int32 i =
    [ word <| Bitwise.shiftRightBy 24 i
    , word <| Bitwise.shiftRightBy 16 i
    , word <| Bitwise.shiftRightBy 8 i
    , word i
    ]


word : Int -> Int
word =
    Bitwise.and 0xFF
