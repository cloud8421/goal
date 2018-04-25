module State exposing (..)

import Api
import Dict
import RemoteData exposing (RemoteData(..))
import Types exposing (..)


init : ( Model, Cmd Msg )
init =
    { projects = NotAsked } ! [ Api.getProjects ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        GetProjects ->
            model ! [ Api.getProjects ]

        ProjectsResponse projects ->
            let
                transformer ps =
                    ps
                        |> List.map (\p -> ( p.id, p ))
                        |> Dict.fromList
            in
            { model | projects = RemoteData.map transformer projects } ! []

        GetProjectDetails projectId ->
            model ! [ Api.getProject projectId ]

        ProjectDetailResponse project ->
            let
                dbg =
                    Debug.log "project" project
            in
            model ! []
