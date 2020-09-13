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



-- Should be Uuid.Uuid, but Elm doesn't see those as Comparable.


type alias BoardState =
    ( List Id, List Id, List Id )


type alias CardContents =
    ( String, String )


defaultCard =
    ( "Untitled", "" )


type alias Model =
    { positions : AppendOnlySet (Sequence BoardState)
    , contents : Dict Id (AppendOnlySet (Sequence CardContents))
    , votes : Dict Id (Counter Id)
    , currentSeed : Seed
    , currentUuid : Uuid.Uuid
    }


initBoardState : AppendOnlySet (Sequence BoardState)
initBoardState =
    insertAOS ( 0, ( [], [], [] ) ) <| AppendOnlySet Set.empty


init : ( Int, List Int ) -> ( Model, Cmd Msg )
init ( seed, seedExtension ) =
    let
        firstSeed =
            initialSeed seed seedExtension

        ( newUuid, newSeed ) =
            step Uuid.generator firstSeed
    in
    ( { positions = initBoardState
      , contents = Dict.empty
      , votes = Dict.empty
      , currentSeed = newSeed
      , currentUuid = newUuid
      }
    , Cmd.none
    )


type Msg
    = NewCard
    | AdvanceCard String



-- TODO: On change to the model, dispatch CRDT updates to websockets.


update msg model =
    case msg of
        NewCard ->
            let
                id =
                    Uuid.toString model.currentUuid

                ( td, ip, dn ) =
                    withDefault ( [], [], [] ) <| latest model.positions

                posSeqNum =
                    withDefault 0 <| seqNum model.positions

                positions : AppendOnlySet (Sequence BoardState)
                positions =
                    insertAOS ( posSeqNum + 1, ( List.append td [ id ], ip, dn ) ) model.positions

                contents : Dict Id (AppendOnlySet (Sequence CardContents))
                contents =
                    Dict.insert id (AppendOnlySet Set.empty) model.contents

                ( newUuid, newSeed ) =
                    step Uuid.generator model.currentSeed
            in
            ( { model
                | positions = positions
                , contents = contents
                , currentSeed = newSeed
                , currentUuid = newUuid
              }
            , Cmd.none
            )

        AdvanceCard id ->
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


mapCards f column model =
    let
        boardState =
            withDefault ( [], [], [] ) <| latest model.positions

        colIds =
            getColumn column boardState

        contents =
            model.contents

        cardBodies =
            Dict.map
                (\k v ->
                    v
                        |> latest
                        |> withDefault defaultCard
                )
                contents
    in
    List.map
        (\k ->
            case Dict.get k cardBodies of
                Nothing ->
                    f k defaultCard

                Just t ->
                    f k t
        )
        colIds



-- TODO: Manipulating all the CRDTs may be messy; would it make sense to write a function that just
-- collapses everything down to one "latest" state for rendering?


cardView : String -> CardContents -> Element Msg
cardView id contents =
    let
        ( title, body ) =
            contents
    in
    button [] { label = text title, onPress = Just <| AdvanceCard id }


cardsView : Column -> Model -> List (Element Msg)
cardsView column model =
    mapCards cardView column model


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
                    ++ cardsView ToDiscuss model
                    ++ [ button [ Font.size 12, Font.italic ]
                            { label = text "+ New Card", onPress = Just NewCard }
                       ]
            , column [ width <| fillPortion 1 ] <|
                [ el [ Font.underline ] <| text "In Progress" ]
                    ++ cardsView InProgress model
            , column [ width <| fillPortion 1 ] <|
                [ el [ Font.underline ] <| text "Done" ]
                    ++ cardsView Done model
            ]
