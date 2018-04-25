module State exposing (..)

import Api
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
            { model | projects = projects } ! []
