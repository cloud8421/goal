module Types exposing (..)

import Dict exposing (Dict)
import RemoteData exposing (WebData)


type alias Project =
    { id : Int
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


type alias Model =
    { projects : Projects }


type Msg
    = NoOp
    | GetProjects
    | ProjectsResponse (WebData (List Project))
    | GetProjectDetails Int
    | ProjectDetailResponse (WebData Project)
