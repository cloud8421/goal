module View exposing (..)

import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Dict
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
    column columnModifiers [] [ projectsList model.projects ]


goalsColumn : Model -> Html Msg
goalsColumn model =
    column columnModifiers [] [ goalsList model.goals ]


actionsColumn : Model -> Html msg
actionsColumn model =
    column columnModifiers [] [ title H3 [] [ text "Actions" ] ]


projectsList : Projects -> Html Msg
projectsList projects =
    let
        panelTop =
            [ panelHeading [] [ title H4 [] [ text "Projects" ] ]
            ]

        panelLinkItem item =
            panelLink False
                []
                [ p [ onClick (GetProjectGoals item.id) ]
                    [ text item.name
                    ]
                ]
    in
    case projects of
        R.Success items ->
            panel []
                (panelTop ++ List.map panelLinkItem (Dict.values items))

        R.Loading ->
            title H4 [] [ text "Loading..." ]

        otherwise ->
            title H4 [] [ text "N/A" ]


goalsList : Goals -> Html Msg
goalsList goals =
    let
        panelTop =
            [ panelHeading [] [ title H4 [] [ text "Goals" ] ]
            ]

        panelLinkItem item =
            panelLink False
                []
                [ p []
                    [ text item.description
                    ]
                ]
    in
    case goals of
        R.Success items ->
            panel []
                (panelTop ++ List.map panelLinkItem (Dict.values items))

        R.Loading ->
            title H4 [] [ text "Loading..." ]

        otherwise ->
            title H4 [] [ text "N/A" ]
