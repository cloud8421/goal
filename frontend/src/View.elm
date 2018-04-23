module View exposing (..)

import Bulma.Columns exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Html exposing (Html, main_, text)
import Types exposing (..)


root : Model -> Html Msg
root model =
    main_ []
        [ exampleHero model
        , exampleColumns
        ]


exampleHero : Model -> Html msg
exampleHero model =
    hero { heroModifiers | size = Medium, color = Primary }
        []
        [ heroBody []
            [ container []
                [ title H1 [] [ text "Hero Title" ]
                , title H2 [] [ model |> toString |> text ]
                ]
            ]
        ]


exampleColumns : Html msg
exampleColumns =
    section NotSpaced
        []
        [ container []
            [ columns columnsModifiers
                []
                [ column columnModifiers [] [ text "First Column" ]
                , column columnModifiers [] [ text "Second Column" ]
                , column columnModifiers [] [ text "Third Column" ]
                ]
            ]
        ]
