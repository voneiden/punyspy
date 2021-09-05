module Main exposing (main)

import Browser
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)

type alias ConnectionStats =
    { messagesIn : Int
    , messagesOut: Int
    , created: Int
    , ping: Int
    , sockAddr: String
    }

connectionStatsDecoder : Decoder ConnectionStats
connectionStatsDecoder =
    Decode.succeed ConnectionStats
        |> required "messagesIn" int
        |> required "messagesOut" int
        |> required "created" int
        |> required "ping" int
        |> required "sockAddr" string

type alias Message =
    { topic : List String
    , message : String
    }

messageDecoder : Decoder Message
messageDecoder =
    Decode.succeed Message
        |> required "topic" (list string)
        |> required "message" string

type alias StatsResponse =
    { connectionStats : List ConnectionStats
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
    }

type  Model = INIT | READY State | ERROR

type Msg
  = GotStats (Result Http.Error StatsResponse)

init :  () -> (Model, Cmd Msg)
init _ =
  (INIT, getStats)

update : Msg -> Model -> (Model, Cmd a)
update msg model =
    case msg of
        GotStats result ->
            case result of
                Ok stats ->
                    (READY {stats=stats}, Cmd.none)
                Err _ ->
                    (ERROR, Cmd.none)

view : Model -> Document a
view m = {title = "test", body = [Html.div [] [Html.text "Hello"]]}

subscriptions : Model -> Sub a
subscriptions m = Sub.none


getStats : Cmd Msg
getStats =
  Http.get
    { url = "http://localhost:18080/stats"
    , expect = Http.expectJson GotStats statsResponseDecoder
    }
