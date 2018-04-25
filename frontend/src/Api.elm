module Api exposing (..)

import Decode as D
import Http
import Platform.Cmd as Cmd
import RemoteData
import Types exposing (..)


getProjects : Cmd Msg
getProjects =
    Http.get "/api/projects" D.projectsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map ProjectsResponse


getProject : Int -> Cmd Msg
getProject projectId =
    Http.get ("/api/projects/" ++ toString projectId) D.projectDecoder
        |> RemoteData.sendRequest
        |> Cmd.map ProjectDetailResponse
