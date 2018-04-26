module Store exposing (..)

import Dict exposing (Dict)


type alias Item a =
    { a | id : Int }


type alias Store a =
    Dict Int (Item a)


empty : Store (Item a)
empty =
    Dict.empty


fromList : List (Item a) -> Store (Item a)
fromList coll =
    coll
        |> List.map (\i -> ( i.id, i ))
        |> Dict.fromList
