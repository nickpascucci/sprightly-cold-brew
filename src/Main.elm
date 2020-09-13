module Main exposing (..)

import Browser
import Crdt exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html, div)
import Maybe exposing (withDefault)
import Prng.Uuid as Uuid
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Set exposing (Set)


main : Program ( Int, List Int ) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


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
    , currentSeed : Seed
    , currentUuid : Maybe Uuid.Uuid
    }


initBoardState : AppendOnlySet (Sequence BoardState)
initBoardState =
    AppendOnlySet Set.empty


init : ( Int, List Int ) -> ( Model, Cmd Msg )
init ( seed, seedExtension ) =
    ( { positions = initBoardState
      , contents = Dict.empty
      , votes = Dict.empty
      , currentSeed = initialSeed seed seedExtension
      , currentUuid = Nothing
      }
    , Cmd.none
    )


type Msg
    = NewCard



-- TODO: Add UUID generation logic, then wire up the NewCard action so that it adds the UUID to the
-- "To Discuss" column state and a default CardContents to the contents state.
-- TODO: On change to the model, dispatch CRDT updates to websockets.


update msg model =
    case msg of
        NewCard ->
            ( model, Cmd.none )


type Column
    = ToDiscuss
    | InProgress
    | Done


getColumn column boardState =
    let
        ( td, ip, dn ) =
            boardState
    in
    case column of
        ToDiscuss ->
            td

        InProgress ->
            ip

        Done ->
            dn


cardTitles column model =
    let
        boardState =
            withDefault ( [], [], [] ) <| latest model.positions

        colIds =
            getColumn column boardState

        contents =
            model.contents

        titles =
            Dict.map
                (\k v ->
                    v
                        |> latest
                        |> withDefault ( "Untitled", "Ignored" )
                        |> Tuple.first
                )
                contents
    in
    List.map
        (\k ->
            case Dict.get k titles of
                Nothing ->
                    text "Untitled"

                Just t ->
                    text t
        )
        colIds



-- TODO: Manipulating all the CRDTs may be messy; would it make sense to write a function that just
-- collapses everything down to one "latest" state for rendering?


view model =
    layout [] <|
        row [ width fill, height fill ]
            [ column
                [ width <| fillPortion 1
                , Background.color <| rgb255 92 99 118
                , Font.color <| rgb255 255 255 255
                ]
              <|
                [ el [ Font.underline ] <| text "To Discuss" ]
                    ++ cardTitles ToDiscuss model
                    ++ [ button [ Font.size 12, Font.italic ]
                            { label = text "+ New Card", onPress = Just NewCard }
                       ]
            , column [ width <| fillPortion 1 ] [ el [ Font.underline ] <| text "In Progress" ]
            , column [ width <| fillPortion 1 ] [ el [ Font.underline ] <| text "Done" ]
            ]
