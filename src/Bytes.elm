module Bytes
    exposing
        ( (<>)
        , Bytes
        , concat
        , empty
        , int32
        , int8
        , length
        , pad
        , string
        , toList
        )

import Bitwise
import Char


type Bytes
    = Bytes (List Int)


toList : Bytes -> List Int
toList (Bytes ints) =
    ints


length : Bytes -> Int
length (Bytes ints) =
    List.length ints


empty : Bytes
empty =
    Bytes []


concat : List Bytes -> Bytes
concat =
    List.map toList >> List.concat >> Bytes


string : String -> Bytes
string =
    String.toList >> List.map Char.toCode >> Bytes


int8 : Int -> Bytes
int8 i =
    Bytes [ pack i ]


int32 : Int -> Bytes
int32 i =
    Bytes
        [ pack <| Bitwise.shiftRightBy 24 i
        , pack <| Bitwise.shiftRightBy 16 i
        , pack <| Bitwise.shiftRightBy 8 i
        , pack i
        ]


pack : Int -> Int
pack =
    Bitwise.and 0xFF


pad : Int -> Int -> Bytes
pad n size =
    case size % n of
        0 ->
            Bytes <| []

        r ->
            Bytes <| List.repeat (n - r) 0


(<>) : Bytes -> Bytes -> Bytes
(<>) (Bytes x) (Bytes y) =
    Bytes (x ++ y)
infixr 5 <>
