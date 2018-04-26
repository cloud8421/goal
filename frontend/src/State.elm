module State exposing (..)

import Api
import Dict exposing (Dict)
import RemoteData exposing (RemoteData(..))
import Types exposing (..)


init : ( Model, Cmd Msg )
init =
    { projects = NotAsked
    , goals = NotAsked
    , actions = NotAsked
    , currentProject = Nothing
    }
        ! [ Api.getProjects ]


type alias Collectable a =
    { a | id : Int }


intoDict : List (Collectable a) -> Dict Int (Collectable a)
intoDict collection =
    collection
        |> List.map (\i -> ( i.id, i ))
        |> Dict.fromList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        GetProjects ->
            model ! [ Api.getProjects ]

        ProjectsResponse projects ->
            { model | projects = RemoteData.map intoDict projects } ! []

        GetProjectGoals projectId ->
            { model
                | currentProject = Just projectId
                , goals = Loading
            }
                ! [ Api.getProjectGoals projectId ]

        ProjectGoalsResponse goals ->
            { model | goals = RemoteData.map intoDict goals } ! []
