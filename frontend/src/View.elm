module View exposing (..)

import Html exposing (Html, div, text)
import Types exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ model |> toString |> text ]
