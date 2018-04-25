module Types exposing (..)

import RemoteData exposing (WebData)


type Msg
    = NoOp
    | GetProjects
    | ProjectsResponse (WebData (List Project))


type alias Model =
    { projects : WebData (List Project) }


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
