module State exposing (..)

import Api
import Dict exposing (Dict)
import RemoteData exposing (RemoteData(..))
import Types exposing (..)


init : ( Model, Cmd Msg )
init =
    { projects = NotAsked } ! [ Api.getProjects ]


intoDict : List Project -> Dict Int Project
intoDict collection =
    collection
        |> List.map (\p -> ( p.id, p ))
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

        GetProjectDetails projectId ->
            model ! [ Api.getProject projectId ]

        ProjectDetailResponse project ->
            let
                addProject p ps =
                    Dict.insert p.id p ps

                newProjects =
                    RemoteData.map2 addProject project model.projects
            in
            { projects = newProjects } ! []
