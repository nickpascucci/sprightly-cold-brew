module Main exposing (..)

import Browser
import Crdt exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Set exposing (Set)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Id =
    String


type alias BoardState =
    ( List Id, List Id, List Id )


type alias CardContents =
    ( String, String )


type alias Model =
    { positions : AppendOnlySet (Sequence BoardState)
    , contents : Dict Id (AppendOnlySet (Sequence CardContents))
    , votes : Dict Id (Counter Id)
    }


initBoardState : AppendOnlySet (Sequence BoardState)
initBoardState =
    AppendOnlySet Set.empty


init : Model
init =
    { positions = initBoardState
    , contents = Dict.empty
    , votes = Dict.empty
    }


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model

        Decrement ->
            model


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text "Hello, world" ]
        , button [ onClick Increment ] [ text "+" ]
        ]
