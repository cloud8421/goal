module Types exposing (..)

import Dict exposing (Dict)
import RemoteData exposing (WebData)


type alias ProjectId =
    Int


type alias Project =
    { id : ProjectId
    , name : String
    , goals : WebData (List Goal)
    }


type alias Goal =
    { id : Int
    , description : String
    , actions : WebData (List Action)
    }


type alias Action =
    { id : Int
    , summary : String
    }


type alias Projects =
    WebData (Dict Int Project)


type alias Goals =
    WebData (Dict Int Goal)


type alias Actions =
    WebData (Dict Int Action)


type alias Model =
    { projects : Projects
    , goals : Goals
    , actions : Actions
    , currentProject : Maybe Int
    }


type Msg
    = NoOp
    | GetProjects
    | ProjectsResponse (WebData (List Project))
    | GetProjectGoals ProjectId
    | ProjectGoalsResponse (WebData (List Goal))
