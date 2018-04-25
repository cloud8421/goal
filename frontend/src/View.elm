module View exposing (..)

import Bulma.Columns exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Html exposing (Html, main_, p, text)
import Html.Events exposing (onClick)
import RemoteData as R
import Types exposing (..)


root : Model -> Html Msg
root model =
    main_ []
        [ topHero model
        , activityColumns model
        ]


topHero : Model -> Html msg
topHero model =
    hero { heroModifiers | size = Small, color = Primary }
        []
        [ heroBody []
            [ container []
                [ title H1 [] [ text "Goals" ]
                ]
            ]
        ]


activityColumns : Model -> Html Msg
activityColumns model =
    section NotSpaced
        []
        [ container []
            [ columns columnsModifiers
                []
                [ projectsColumn model
                , goalsColumn model
                , actionsColumn model
                ]
            ]
        ]


projectsColumn : Model -> Html Msg
projectsColumn model =
    column columnModifiers
        []
        [ title H3 [] [ text "Projects" ]
        , projectsList model.projects
        ]


goalsColumn : Model -> Html msg
goalsColumn model =
    column columnModifiers [] [ title H3 [] [ text "Goals" ] ]


actionsColumn : Model -> Html msg
actionsColumn model =
    column columnModifiers [] [ title H3 [] [ text "Actions" ] ]


projectsList : Projects -> Html Msg
projectsList projects =
    let
        tableHeaders =
            [ tableCellHead [] [ text "Name" ] ]

        projectRow item =
            tableRow False
                []
                [ tableCell []
                    [ p [ onClick (GetProjectDetails item.id) ] [ text item.name ]
                    ]
                ]
    in
    case projects of
        R.Success items ->
            container []
                [ table tableModifiers
                    []
                    [ tableHead [] tableHeaders
                    , tableBody [] (List.map projectRow items)
                    ]
                ]

        otherwise ->
            container
                []
                [ title H4 [] [ text "N/A" ]
                ]
