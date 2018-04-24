module Types exposing (..)

import RemoteData exposing (WebData)


type Msg
    = NoOp


type alias Model =
    { projects : List Project }


type alias Project =
    { id : String
    , name : String
    , goals : WebData (List Goal)
    }


type alias Goal =
    { id : String
    , description : String
    , actions : WebData (List Action)
    }


type alias Action =
    { id : String
    , summary : String
    }
