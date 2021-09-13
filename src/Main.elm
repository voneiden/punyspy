port module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Html exposing (style)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Task
import Time

type alias ConnectionStat =
    { messagesIn : Int
    , messagesOut: Int
    , created: Int
    , ping: Int
    , sockAddr: String
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
    { id : Int
    , timestamp : Time.Posix
    , topic : List String
    , message : String
    }

type alias StoredMessage =
    { data : Message
    , received: Time.Posix
    }


storeMessage : Time.Posix -> Message -> StoredMessage
storeMessage now msg =
    { data=msg, received=now }

messageDecoder : Decoder Message
messageDecoder =
    Decode.succeed Message
        |> required "id" int
        |> required "timestamp" (int |> andThen (\t -> Decode.succeed <| Time.millisToPosix t))
        |> required "topic" (list string)
        |> required "message" string


messageEncode : Message -> Json.Encode.Value
messageEncode message =
    Json.Encode.object [
        ("test", Json.Encode.string message.message )
    ]
messagesEncode : List Message -> Json.Encode.Value
messagesEncode messages =
    Json.Encode.list messageEncode messages


type alias StatsResponse =
    { connectionStats : List ConnectionStat
    , messages : List Message
    }

statsResponseDecoder : Decoder StatsResponse
statsResponseDecoder =
    Decode.succeed StatsResponse
        |> required "connectionStats" (list connectionStatsDecoder)
        |> required "messages" (list messageDecoder)

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
    , messages : List Message
    }

type  Model = INIT (Maybe Time.Posix) | READY State | ERROR

type Msg
  = GotStats (Result Http.Error StatsResponse)
  | Tick Time.Posix

init :  () -> (Model, Cmd Msg)
init _ =
  (INIT Nothing, Cmd.batch [getStats 0, getTime])

lastId : List Message -> Int
lastId messages =
    case List.head <| List.reverse messages of
        Just message ->
            message.id
        Nothing ->
            0

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotStats result ->
            case result of
                Ok stats ->
                    case model of
                        READY state ->
                            (READY {state | stats=stats, messages=(state.messages ++ stats.messages)}, Cmd.none)
                        INIT (Just t) ->
                            (READY {stats=stats, time=t, messages=stats.messages}, Cmd.none)
                        _ ->
                            -- Da fuq
                            (READY {stats=stats, time=Time.millisToPosix 0, messages=stats.messages}, Cmd.none)
                Err _ ->
                    (ERROR, Cmd.none)

        Tick t ->
            case model of
                READY stats ->
                    (READY {stats | time=t}, getStats (lastId stats.messages))
                INIT _ ->
                    (INIT (Just t), Cmd.none)
                _ ->
                    (model, Cmd.none)

uptime : Time.Posix -> Time.Posix -> String
uptime now t =
    let
        diff = (Time.posixToMillis now - Time.posixToMillis t) // 1000
    in
    if diff < 60
    then String.fromInt diff ++ "s"
    else
        if diff < 3600
        then
            String.fromInt (diff // 60) ++ "m " ++ String.fromInt (modBy 60 diff) ++ "s"
        else
            String.fromInt (diff // 3600) ++ "h " ++ String.fromInt (modBy 60 <| diff // 3600) ++ "m " ++ String.fromInt (modBy 60 diff) ++ "s"
viewConnectionStat : Time.Posix -> ConnectionStat -> Html Msg
viewConnectionStat now stat =
    Html.tr [] [
        Html.td [] [Html.text stat.sockAddr],
        Html.td [] [Html.text <| String.fromInt stat.ping],
        Html.td [] [Html.text <| uptime now <| Time.millisToPosix stat.created],
        Html.td [] [Html.text <| String.fromInt stat.messagesIn],
        Html.td [] [Html.text <| String.fromInt stat.messagesOut]
    ]

viewConnectionStats : Time.Posix -> List ConnectionStat -> Html Msg
viewConnectionStats now stats =
    Html.div [] [
        Html.h1 [] [Html.text "Active connections"],
        Html.table [] [
            Html.thead [] [
                Html.th [] [Html.text "SockName"],
                Html.th [] [Html.text "Ping"],
                Html.th [] [Html.text "Uptime"],
                Html.th [] [Html.text "Messages in"],
                Html.th [] [Html.text "Messages out"]
            ],
            Html.tbody [] (List.map (viewConnectionStat now) stats)
        ]
    ]

viewMessage : Message -> Html Msg
viewMessage message =
    Html.div [] [Html.text <| (String.join "/" message.topic) ++  message.message]

viewMessages : List Message -> Html Msg
viewMessages messages =
    Html.div [] <| [
        Html.h1 [] [Html.text "Message list"]
    ] ++ List.map viewMessage messages


view : Model -> Document Msg
view model = case model of
    READY state ->
        {title = "SpyBus 2000", body = [Html.div [] [viewConnectionStats state.time state.stats.connectionStats, viewMessages state.messages ]]}
    INIT _ ->
        {title = "SpyBus 2000 - Loading", body = [Html.div [] [Html.text "Booting up, hold on to your pants"] ]}
    ERROR ->
        {title = "SpyBus 2000 - Error", body = [Html.div [] [Html.text "Fatal error has occured" ]]}

subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch [
        Time.every 1000 Tick
    ]


-- PORTS
-- check https://ellie-app.com/8yYddD6HRYJa1
port setStorage : Json.Encode.Value -> Cmd msg

getStats : Int -> Cmd Msg
getStats since =
  Http.get
    { url = "http://localhost:18080/stats?since=" ++ String.fromInt since
    , expect = Http.expectJson GotStats statsResponseDecoder
    }

getTime : Cmd Msg
getTime =
    Task.perform Tick Time.now

