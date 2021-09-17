port module Main exposing (main)

import Base64.Encode as Encode
import Base64.Decode as B64Decode
import Browser
import Char exposing (fromCode)
import Html exposing (Attribute, Html, button, div, h1, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, id, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Http
import Json.Decode as Decode exposing (Decoder, andThen, decodeString, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import String exposing (fromChar)
import Task
import Time

-- TODO TODO TODO!
-- Message must be parsed into bytes, we cannot expect it to be utf8 string, that's just gonna be a bad day

type alias ConnectionStat =
    { messagesIn : Int
    , messagesOut : Int
    , created : Int
    , ping : Int
    , sockAddr : String
    }


connectionStatsDecoder : Decoder ConnectionStat
connectionStatsDecoder =
    Decode.succeed ConnectionStat
        |> required "messagesIn" int
        |> required "messagesOut" int
        |> required "created" int
        |> required "ping" int
        |> required "sockAddr" string


type alias Message =
    { id : String
    , timestamp : Time.Posix
    , topic : List String
    , message : String
    }


type alias SourceMessage =
    { timestamp : Time.Posix
    , topic : List String
    , message : String
    }


convertMessage : String -> SourceMessage -> Message
convertMessage id source =
    { id = id, timestamp = source.timestamp, topic = source.topic, message = source.message }


appendMessage : SourceMessage -> List Message -> List Message
appendMessage source messages =
    convertMessage (String.fromInt <| List.length messages) source :: messages


b64DecodeDefault : String -> String
b64DecodeDefault s =
    case B64Decode.decode B64Decode.string s of
        Ok ds ->
            ds
        Err _ ->
            "Decode failed"


messageDecoder : Decoder SourceMessage
messageDecoder =
    Decode.succeed SourceMessage
        |> required "timestamp" (int |> andThen (\t -> Decode.succeed <| Time.millisToPosix t))
        |> required "topic" (list string)
        |> required "message" (string |> andThen (\m -> Decode.succeed <| b64DecodeDefault m))


messageEncode : Message -> Json.Encode.Value
messageEncode message =
    Json.Encode.object
        [ ( "test", Json.Encode.string message.message )
        ]


messagesEncode : List Message -> Json.Encode.Value
messagesEncode messages =
    Json.Encode.list messageEncode messages


type alias StatsResponse =
    { connectionStats : List ConnectionStat
    }


statsResponseDecoder : Decoder StatsResponse
statsResponseDecoder =
    Decode.succeed StatsResponse
        |> required "connectionStats" (list connectionStatsDecoder)



--- result : Result String User
--result =
--    Decode.decodeString
--        userDecoder


type alias Document a =
    { title : String
    , body : List (Html a)
    }


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


type alias State =
    { stats : StatsResponse
    , time : Time.Posix
    , zone : Time.Zone
    , messages : List Message
    , inputDevice : String
    , inputModule : String
    , inputAddress : String
    }


type Model
    = INIT (Maybe StatsResponse) (Maybe Time.Posix) (Maybe Time.Zone) (List Message)
    | READY State
    | ERROR
    | ERROR_DECODE Decode.Error


type Msg
    = GotStats (Result Http.Error StatsResponse)
    | Tick Time.Posix
    | Zone Time.Zone
    | Write
    | Read
    | InputDevice String
    | InputModule String
    | InputAddress String
    | CmdSent (Result Http.Error ())
    | SendWSMessage String
    | ReceiveWSMessage String
    | CheckInitReady


init : () -> ( Model, Cmd Msg )
init _ =
    ( INIT Nothing Nothing Nothing [], Cmd.batch [ getStats, getTime, getZone ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        READY state ->
            case msg of
                GotStats result ->
                    case result of
                        Ok stats ->
                            ( READY { state | stats = stats }, Cmd.none )

                        Err _ ->
                            ( ERROR, Cmd.none )

                Tick t ->
                    ( READY { state | time = t }, getStats )

                Write ->
                    let
                        message =
                            Encode.encode <| Encode.string <| fromChar <| fromCode 0
                    in
                    ( model, postPub ("sx/" ++ state.inputDevice ++ "/i/w/" ++ state.inputModule ++ "/" ++ state.inputAddress ++ "/") message )

                Read ->
                    let
                        message =
                            Encode.encode <| Encode.string <| fromChar <| fromCode 1
                    in
                    ( model, postPub ("sx/" ++ state.inputDevice ++ "/i/r/" ++ state.inputModule ++ "/" ++ state.inputAddress ++ "/") message )

                InputDevice inputDevice ->
                    ( READY { state | inputDevice = inputDevice }, Cmd.none )

                InputModule inputModule ->
                    ( READY { state | inputModule = inputModule }, Cmd.none )

                InputAddress inputAddress ->
                    ( READY { state | inputAddress = inputAddress }, Cmd.none )

                CmdSent _ ->
                    ( model, Cmd.none )

                SendWSMessage wsmsg ->
                    ( model, sendWSMessage wsmsg )

                ReceiveWSMessage wsmsg ->
                    case decodeString messageDecoder wsmsg of
                        Ok decodedMessage ->
                            ( READY { state | messages = appendMessage decodedMessage state.messages }, Cmd.none )

                        Err error ->
                            ( ERROR_DECODE error, Cmd.none )

                CheckInitReady ->
                    ( model, Cmd.none )

                Zone zone ->
                    ( READY { state | zone = zone }, Cmd.none )

        INIT maybeStats maybeTime maybeZone messages ->
            case msg of
                GotStats result ->
                    case result of
                        Ok stats ->
                            update CheckInitReady (INIT (Just stats) maybeTime maybeZone messages)

                        Err _ ->
                            ( ERROR, Cmd.none )

                Tick t ->
                    update CheckInitReady (INIT maybeStats (Just t) maybeZone messages)

                Zone z ->
                    update CheckInitReady (INIT maybeStats maybeTime (Just z) messages)

                ReceiveWSMessage wsmsg ->
                    case decodeString messageDecoder wsmsg of
                        Ok decodedMessage ->
                            ( INIT maybeStats maybeTime maybeZone (appendMessage decodedMessage messages), Cmd.none )

                        Err error ->
                            ( ERROR_DECODE error, Cmd.none )

                CheckInitReady ->
                    case model of
                        INIT (Just stats) (Just time) (Just zone) _ ->
                            ( READY (State stats time zone messages "" "" ""), Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ERROR ->
            ( model, Cmd.none )

        ERROR_DECODE _ ->
            ( model, Cmd.none )


uptime : Time.Posix -> Time.Posix -> String
uptime now t =
    let
        diff =
            (Time.posixToMillis now - Time.posixToMillis t) // 1000
    in
    if diff < 60 then
        String.fromInt diff ++ "s"

    else if diff < 3600 then
        String.fromInt (diff // 60) ++ "m " ++ String.fromInt (modBy 60 diff) ++ "s"

    else
        String.fromInt (diff // 3600) ++ "h " ++ String.fromInt (modBy 60 <| diff // 3600) ++ "m " ++ String.fromInt (modBy 60 diff) ++ "s"


viewConnectionStat : Time.Posix -> ConnectionStat -> Html Msg
viewConnectionStat now stat =
    tr []
        [ td [] [ text stat.sockAddr ]
        , td [] [ text <| String.fromInt stat.ping ]
        , td [] [ text <| uptime now <| Time.millisToPosix stat.created ]
        , td [] [ text <| String.fromInt stat.messagesIn ]
        , td [] [ text <| String.fromInt stat.messagesOut ]
        ]


viewConnectionStats : Time.Posix -> List ConnectionStat -> Html Msg
viewConnectionStats now stats =
    div [ id "stats" ]
        [ h1 [] [ text "Active connections" ]
        , table []
            [ thead []
                [ th [] [ text "SockName" ]
                , th [] [ text "Ping" ]
                , th [] [ text "Uptime" ]
                , th [] [ text "Messages in" ]
                , th [] [ text "Messages out" ]
                ]
            , tbody [] (List.map (viewConnectionStat now) stats)
            ]
        ]


timeToString : Int -> String
timeToString n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n


viewMessageTimestamp : Time.Zone -> Time.Posix -> Html Msg
viewMessageTimestamp zone t =
    let
        hour =
            timeToString <| Time.toHour zone t

        minute =
            timeToString <| Time.toMinute zone t

        second =
            timeToString <| Time.toSecond zone t

        milli =
            timeToString <| Time.toMillis zone t
    in
    div [ class "message-timestamp" ] [ text <| hour ++ ":" ++ minute ++ ":" ++ second ++ "." ++ milli ]


viewMessageBody : String -> Html Msg
viewMessageBody message =
    div [ class "message-body" ] [ text <| String.join " " <| List.map String.fromInt <|List.map Char.toCode <| String.toList message ]


viewMessageTopic : List String -> Html Msg
viewMessageTopic topic =
    div [ class "message-topic" ] [ text <| String.join "/" topic ]


messageSxClass : List String -> Attribute Msg
messageSxClass topic =
    case List.head <| List.drop 2 topic of
        Just "o" ->
            class "m-output"

        Just "i" ->
            class "m-input"

        _ ->
            class "m-unknown"


messageClass : Message -> List (Attribute Msg)
messageClass message =
    case List.head message.topic of
        Just "sx" ->
            [ class "sx", messageSxClass message.topic ]

        _ ->
            [ class "m-default" ]


viewMessage : Time.Zone -> Message -> ( String, Html Msg )
viewMessage zone message =
    ( message.id ++ (String.fromInt <| Time.posixToMillis message.timestamp)
    , div ([ class "message" ] ++ messageClass message)
        [ viewMessageTimestamp zone message.timestamp
        , viewMessageTopic message.topic
        , viewMessageBody message.message
        ]
    )


viewMessages : State -> Html Msg
viewMessages state =
    div [ id "messages" ] <|
        [ h1 [] [ text "Message list" ]
        , viewInput state
        , Keyed.node "div" [ class "messages-container" ] (List.map (viewMessage state.zone) state.messages)
        ]


viewInput : State -> Html Msg
viewInput state =
    div []
        [ input [ placeholder "Device", value state.inputDevice, onInput InputDevice ] []
        , input [ placeholder "Module", value state.inputModule, onInput InputModule ] []
        , input [ placeholder "Address", value state.inputAddress, onInput InputAddress ] []
        , button [ onClick Read ] [ text "Read" ]
        , button [ onClick Write ] [ text "Write" ]
        ]


view : Model -> Document Msg
view model =
    case model of
        INIT _ _ _ _ ->
            { title = "SpyBus 2000 - Booting up", body = [ div [] [ text "Hold on to your pants" ] ] }

        READY state ->
            { title = "SpyBus 2000", body = [ div [ id "root" ] [ viewConnectionStats state.time state.stats.connectionStats, viewMessages state ] ] }

        ERROR ->
            { title = "SpyBus 2000 - Error", body = [ div [] [ text "Fatal error has occured" ] ] }

        ERROR_DECODE error ->
            { title = "SpyBus 2000 - Error", body = [ div [] [ text <| "Fatal json decode error has occured: " ++ Decode.errorToString error ] ] }


port sendWSMessage : String -> Cmd msg


port receiveWSMessage : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ Time.every 1000 Tick
        , receiveWSMessage ReceiveWSMessage
        ]


getStats : Cmd Msg
getStats =
    Http.get
        { url = "http://localhost:18080/stats"
        , expect = Http.expectJson GotStats statsResponseDecoder
        }


pubEncode : String -> String -> Json.Encode.Value
pubEncode pubTopic pubMessage =
    Json.Encode.object
        [ ( "pubTopic", Json.Encode.string pubTopic )
        , ( "pubMessage", Json.Encode.string pubMessage )
        ]


postPub : String -> String -> Cmd Msg
postPub pubTopic pubMessage =
    Http.post
        { url = "http://localhost:18080/cmd"
        , body = Http.jsonBody (pubEncode pubTopic pubMessage)
        , expect = Http.expectWhatever CmdSent
        }


getTime : Cmd Msg
getTime =
    Task.perform Tick Time.now


getZone : Cmd Msg
getZone =
    Task.perform Zone Time.here
