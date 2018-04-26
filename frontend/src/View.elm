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
        panelTop =
            [ panelHeading [] [ title H4 [] [ text "Projects" ] ]
            ]

        panelLinkItem item =
            panelLink False
                []
                [ p [ onClick (GetProjectDetails item.id) ]
                    [ text item.name
                    , text <| toString <| item.goals
                    ]
                ]
    in
    case projects of
        R.Success items ->
            panel []
                (panelTop ++ List.map panelLinkItem (Dict.values items))

        otherwise ->
            title H4 [] [ text "N/A" ]
