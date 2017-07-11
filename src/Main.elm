port module Main exposing (..)

import Codec.Beam as Beam
import Dict
import Json.Decode


model : ()
model =
    ()


port done : Beam.ByteArray -> Cmd msg


send : String -> Cmd a
send moduleName =
    done <| Beam.for <| Dict.singleton moduleName 1


main : Program String () a
main =
    Platform.programWithFlags
        { init = send >> (,) model
        , update = \_ _ -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
