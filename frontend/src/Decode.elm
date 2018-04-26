module Decode exposing (..)

import Json.Decode exposing (Decoder, int, list, map, string)
import Json.Decode.Pipeline exposing (decode, optional, required)
import RemoteData exposing (RemoteData(..))
import Types exposing (..)


actionDecoder : Decoder Action
actionDecoder =
    decode Action
        |> required "id" int
        |> required "summary" string


goalDecoder : Decoder Goal
goalDecoder =
    decode Goal
        |> required "id" int
        |> required "description" string
        |> optional "actions" (map Success (list actionDecoder)) NotAsked


projectDecoder : Decoder Project
projectDecoder =
    decode Project
        |> required "id" int
        |> required "name" string
        |> optional "goals" (map Success (list goalDecoder)) NotAsked


projectsDecoder : Decoder (List Project)
projectsDecoder =
    list projectDecoder


goalsDecoder : Decoder (List Goal)
goalsDecoder =
    list goalDecoder
