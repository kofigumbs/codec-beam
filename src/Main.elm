port module Main exposing (..)

import Bytes
import Codec.Beam as Beam
import Json.Decode


model : ()
model =
    ()


port done : List Int -> Cmd msg


send : String -> Cmd a
send moduleName =
    done <| Bytes.toList <| Beam.encode moduleName []


main : Program String () a
main =
    Platform.programWithFlags
        { init = send >> (,) model
        , update = \_ _ -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
