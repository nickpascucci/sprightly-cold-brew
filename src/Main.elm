port module Main exposing (..)

import Browser
import Crdt exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html, div)
import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra as EX
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
        , subscriptions = subscriptions
        }


type alias Id =
    -- Should be Uuid.Uuid, but Elm doesn't see those as Comparable.
    String


type Column
    = ToDiscuss
    | InProgress
    | Done


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
    | MoveCard String Column
    | UpdateCard String Int CardContents
    | Recv String


type UpdateKind
    = CardData
    | CardPosition


updateKindFromString : String -> D.Decoder UpdateKind
updateKindFromString s =
    case s of
        "CardData" ->
            D.succeed CardData

        "CardPosition" ->
            D.succeed CardPosition

        _ ->
            D.fail <| "Trying to decode an UpdateKind but got " ++ s


type alias UpdateData =
    { position : Maybe Column
    , cardData : Maybe CardContents
    }


type alias UpdateMessage =
    -- A CRDT update for, or from, another client.
    { updateKind : UpdateKind
    , id : String
    , seqNum : Int
    , data : UpdateData
    }


cmdFromUpdate : UpdateMessage -> Maybe Msg
cmdFromUpdate upd =
    case upd.updateKind of
        CardData ->
            case upd.data.cardData of
                Just cd ->
                    Just <| UpdateCard upd.id upd.seqNum cd

                Nothing ->
                    Nothing

        CardPosition ->
            case upd.data.position of
                Just pos ->
                    Just <| MoveCard upd.id pos

                Nothing ->
                    Nothing


decode : D.Decoder UpdateMessage
decode =
    -- JSON decoder for reading UpdateMessages
    D.map4 UpdateMessage
        (D.field "updateKind" D.string |> D.andThen updateKindFromString)
        (D.field "id" D.string)
        (D.field "seqNum" D.int)
        (D.field "data" decodeUpdateData)


decodeUpdateData : D.Decoder UpdateData
decodeUpdateData =
    D.map2 UpdateData
        (D.field "position" <| D.maybe (D.string |> D.andThen columnFromString))
        (D.field "cardData" <|
            D.maybe
                (D.map2 Tuple.pair
                    (D.field "title" D.string)
                    (D.field "body" D.string)
                )
        )


columnFromString : String -> D.Decoder Column
columnFromString s =
    case s of
        "ToDiscuss" ->
            D.succeed ToDiscuss

        "InProgress" ->
            D.succeed InProgress

        "Done" ->
            D.succeed Done

        _ ->
            D.fail <| "Trying to decode a Column but got " ++ s


encode : UpdateMessage -> E.Value
encode upd =
    E.object
        [ ( "updateKind"
          , E.string <|
                case upd.updateKind of
                    CardData ->
                        "CardData"

                    CardPosition ->
                        "CardPosition"
          )
        , ( "id", E.string upd.id )
        , ( "seqNum", E.int upd.seqNum )
        , ( "data", encodeData upd.data )
        ]


encodeData : UpdateData -> E.Value
encodeData d =
    E.object
        [ ( "position"
          , EX.maybe
                (\p ->
                    case p of
                        ToDiscuss ->
                            E.string "ToDiscuss"

                        InProgress ->
                            E.string "InProgress"

                        Done ->
                            E.string "Done"
                )
                d.position
          )
        , ( "cardData"
          , EX.maybe
                (\cd ->
                    E.object [ ( "title", E.string <| Tuple.first cd ), ( "body", E.string <| Tuple.second cd ) ]
                )
                d.cardData
          )
        ]


sendUpdate : UpdateMessage -> Cmd msg
sendUpdate upd =
    sendMessage <| encode upd


port sendMessage : E.Value -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- TODO: On change to the model, dispatch CRDT updates to websockets.


update msg model =
    let
        _ =
            Debug.log "Update: " msg
    in
    case msg of
        NewCard ->
            let
                id =
                    Uuid.toString model.currentUuid

                ( td, ip, dn ) =
                    withDefault ( [], [], [] ) <| latest model.positions

                posSeqNum =
                    1 + (withDefault 0 <| seqNum model.positions)

                positions : AppendOnlySet (Sequence BoardState)
                positions =
                    insertAOS ( posSeqNum, ( List.append td [ id ], ip, dn ) ) model.positions

                contents : Dict Id (AppendOnlySet (Sequence CardContents))
                contents =
                    Dict.insert id (AppendOnlySet Set.empty) model.contents

                ( nextUuid, nextSeed ) =
                    step Uuid.generator model.currentSeed
            in
            ( { model
                | positions = positions
                , contents = contents
                , currentSeed = nextSeed
                , currentUuid = nextUuid
              }
            , sendUpdate
                { updateKind = CardData
                , id = id
                , seqNum = posSeqNum
                , data =
                    { position = Nothing
                    , cardData = Just defaultCard
                    }
                }
              -- TODO Send CRDT update
            )

        MoveCard id toCol ->
            let
                curPos =
                    withDefault ( [], [], [] ) <| latest model.positions

                posSeqNum =
                    withDefault 0 <| seqNum model.positions

                nextPos =
                    moveCard id toCol curPos

                positions : AppendOnlySet (Sequence BoardState)
                positions =
                    insertAOS ( posSeqNum + 1, nextPos ) model.positions
            in
            ( { model
                | positions = positions
              }
            , sendUpdate
                { updateKind = CardPosition
                , id = id
                , seqNum = posSeqNum
                , data =
                    { position = Just toCol
                    , cardData = Nothing
                    }
                }
              -- TODO Send CRDT update
            )

        UpdateCard id seqNum contents ->
            let
                cardContents : Maybe (AppendOnlySet (Sequence CardContents))
                cardContents =
                    Dict.get id model.contents

                nextContents =
                    Dict.insert id
                        (insertAOS ( seqNum, contents ) <|
                            withDefault (AppendOnlySet Set.empty) cardContents
                        )
                        model.contents

                ( nextModel, _ ) =
                    case cardContents of
                        Just _ ->
                            (model, Cmd.none)

                        Nothing ->
                            update (MoveCard id ToDiscuss) model
            in
            ( { nextModel
                | contents = nextContents
              }
            , Cmd.none
            )

        Recv s ->
            let
                updateMsg =
                    D.decodeString decode s

                _ =
                    Debug.log "Received: " updateMsg

                ( nextModel, _ ) =
                    case updateMsg of
                        Ok x ->
                            case cmdFromUpdate x of
                                Just m ->
                                    update m model

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )
            in
            ( nextModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv


moveCard id toCol ( td, ip, dn ) =
    let
        isMatch =
            \x -> x /= id

        ( td2, ip2, dn2 ) =
            ( List.filter isMatch td, List.filter isMatch ip, List.filter isMatch dn )
    in
    case toCol of
        ToDiscuss ->
            ( td2 ++ [ id ], ip2, dn2 )

        InProgress ->
            ( td2, ip2 ++ [ id ], dn2 )

        Done ->
            ( td2, ip2, dn2 ++ [ id ] )


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


nextCol col =
    case col of
        ToDiscuss ->
            InProgress

        InProgress ->
            Done

        Done ->
            Done


cardView : Column -> String -> CardContents -> Element Msg
cardView col id contents =
    let
        ( title, body ) =
            contents
    in
    button [] { label = text title, onPress = Just <| MoveCard id (nextCol col) }


cardsView : Column -> Model -> List (Element Msg)
cardsView column model =
    mapCards (cardView column) column model


view model =
    let
        logModel =
            Debug.log "Model: " model
    in
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
