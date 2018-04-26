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


getProjectGoals : ProjectId -> Cmd Msg
getProjectGoals projectId =
    Http.get ("/api/projects/" ++ toString projectId ++ "/goals") D.goalsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map ProjectGoalsResponse
