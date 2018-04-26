module State exposing (..)

import Api
import RemoteData exposing (RemoteData(..))
import Store
import Types exposing (..)


init : ( Model, Cmd Msg )
init =
    { projects = NotAsked
    , goals = NotAsked
    , actions = NotAsked
    , currentProject = Nothing
    }
        ! [ Api.getProjects ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        GetProjects ->
            model ! [ Api.getProjects ]

        ProjectsResponse projects ->
            { model | projects = RemoteData.map Store.fromList projects } ! []

        GetProjectGoals projectId ->
            { model
                | currentProject = Just projectId
                , goals = Loading
            }
                ! [ Api.getProjectGoals projectId ]

        ProjectGoalsResponse goals ->
            { model | goals = RemoteData.map Store.fromList goals } ! []
