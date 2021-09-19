port module Main exposing (main)

import Array
import Base64.Decode as B64Decode
import Base64.Encode as B64Encode
import Browser
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode as BEncode
import Char exposing (fromCode)
import Debug exposing (log)
import Definitions exposing (mcuName, mcuRam, moduleName, moduleTypeFromId, signatureToMCU)
import Dict exposing (Dict)
import Hex exposing (fromHex, maybeToHex, toHex)
import Html exposing (Attribute, Html, button, div, h1, input, li, span, table, tbody, td, text, th, thead, tr, ul)
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
    , message : Maybe (List Int)
    }


type alias SourceMessage =
    { timestamp : Time.Posix
    , topic : List String
    , message : Maybe (List Int)
    }


convertMessage : String -> SourceMessage -> Message
convertMessage id source =
    { id = id, timestamp = source.timestamp, topic = source.topic, message = source.message }


appendMessage : SourceMessage -> List Message -> List Message
appendMessage source messages =
    messages ++ [ convertMessage (String.fromInt <| List.length messages) source ]



--source :: messages
--convertMessage (String.fromInt <| List.length messages) source :: messages


decodeBytesTolist : Int -> Bytes.Decode.Decoder a -> Bytes.Decode.Decoder (List a)
decodeBytesTolist length aDecoder =
    Bytes.Decode.loop ( length, [] ) (decodeBytesTolistStep aDecoder)


decodeBytesTolistStep : Bytes.Decode.Decoder a -> ( Int, List a ) -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, List a ) (List a))
decodeBytesTolistStep elementDecoder ( n, elements ) =
    if n <= 0 then
        Bytes.Decode.succeed (Bytes.Decode.Done (List.reverse elements))

    else
        Bytes.Decode.map (\element -> Bytes.Decode.Loop ( n - 1, element :: elements )) elementDecoder


encodeListToBytes : List Int -> Bytes
encodeListToBytes intBytes =
    BEncode.encode <| BEncode.sequence <| List.map BEncode.unsignedInt8 intBytes


b64DecodeDefault : String -> Maybe (List Int)
b64DecodeDefault s =
    case B64Decode.decode B64Decode.bytes s of
        Ok bytes ->
            Bytes.Decode.decode (decodeBytesTolist (Bytes.width bytes) Bytes.Decode.unsignedInt8) bytes

        Err _ ->
            Nothing


messageDecoder : Decoder SourceMessage
messageDecoder =
    Decode.succeed SourceMessage
        |> required "timestamp" (int |> andThen (\t -> Decode.succeed <| Time.millisToPosix t))
        |> required "topic" (list string)
        |> required "message" (string |> andThen (\m -> Decode.succeed <| b64DecodeDefault m))


type alias StatsResponse =
    { connectionStats : List ConnectionStat
    }


statsResponseDecoder : Decoder StatsResponse
statsResponseDecoder =
    Decode.succeed StatsResponse
        |> required "connectionStats" (list connectionStatsDecoder)


type alias ModuleInfo =
    { sigrow1 : Int
    , sigrow2 : Int
    , sigrow3 : Int
    , stackH : Int
    , stackL : Int
    , error : Int
    , debug1 : Int
    , debug2 : Int
    , debug3 : Int
    , debug4 : Int
    , debug5 : Int
    }


defaultModuleInfo : ModuleInfo
defaultModuleInfo =
    { sigrow1 = 0
    , sigrow2 = 0
    , sigrow3 = 0
    , stackH = 0
    , stackL = 0
    , error = 0
    , debug1 = 0
    , debug2 = 0
    , debug3 = 0
    , debug4 = 0
    , debug5 = 0
    }


type alias Module =
    { id : Int
    , info : ModuleInfo
    }


type alias Device =
    { id : Int
    , modules : Dict Int Module
    }


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
    , devices : Dict Int Device
    , inputDevice : String
    , inputModule : String
    , inputAddress : String
    , inputData : String
    }



-- TODO improve state


type Model
    = INIT (Maybe StatsResponse) (Maybe Time.Posix) (Maybe Time.Zone) (List Message) (Dict Int Device)
    | READY State
    | ERROR_NET Http.Error
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
    | InputData String
    | CmdSent (Result Http.Error ())
    | SendWSMessage String
    | ReceiveWSMessage String
    | CheckInitReady


init : () -> ( Model, Cmd Msg )
init _ =
    ( INIT Nothing Nothing Nothing [] Dict.empty, Cmd.batch [ getStats, getTime, getZone ] )


maybeTriple : ( Maybe Int, Maybe Int, Maybe Int ) -> Maybe ( Int, Int, Int )
maybeTriple maybeints =
    case maybeints of
        ( Just i1, Just i2, Just i3 ) ->
            Just ( i1, i2, i3 )

        _ ->
            Nothing


parseSxTopic : List String -> Maybe ( Int, Int, Int )
parseSxTopic topic =
    case topic of
        "sx" :: deviceId :: "o" :: moduleId :: address :: _ ->
            maybeTriple ( String.toInt deviceId, String.toInt moduleId, String.toInt address )

        _ ->
            Nothing



-- add device
-- update devicemodule error/debug/stack


getDeviceModuleOrDefault : Int -> Device -> Module
getDeviceModuleOrDefault moduleId device =
    case Dict.get moduleId device.modules of
        Just mod ->
            mod

        Nothing ->
            Module moduleId defaultModuleInfo


getDeviceOrDefault : Int -> Dict Int Device -> Device
getDeviceOrDefault deviceId dict =
    case Dict.get deviceId dict of
        Just device ->
            device

        Nothing ->
            Device deviceId Dict.empty


createModules : List Int -> Dict Int Module
createModules moduleIds =
    Dict.fromList <| List.map (\i -> ( i, Module i defaultModuleInfo )) moduleIds


updateDeviceModules : Int -> List Int -> Dict Int Device -> Dict Int Device
updateDeviceModules deviceId moduleIds dict =
    let
        device =
            getDeviceOrDefault deviceId dict
    in
    Dict.insert deviceId { device | modules = createModules moduleIds } dict


updateModuleInfo : Int -> Int -> ModuleInfo -> ModuleInfo
updateModuleInfo address value info =
    case address of
        0xF6 ->
            { info | sigrow1 = value }

        0xF7 ->
            { info | sigrow2 = value }

        0xF8 ->
            { info | sigrow3 = value }

        0xF9 ->
            { info | stackH = value }

        0xFA ->
            { info | stackL = value }

        0xFB ->
            { info | error = value }

        0xFC ->
            { info | debug1 = value }

        0xFD ->
            { info | debug2 = value }

        0xFE ->
            { info | debug3 = value }

        0xFF ->
            { info | debug4 = value }

        _ ->
            info


updateDeviceModuleAddress : Int -> Int -> Int -> List Int -> Dict Int Device -> Dict Int Device
updateDeviceModuleAddress deviceId moduleId address values dict =
    if address >= 0xF6 && address <= 0xFF then
        let
            device =
                getDeviceOrDefault deviceId dict

            mod =
                getDeviceModuleOrDefault moduleId device
        in
        case values of
            [] ->
                dict

            value :: rest ->
                let
                    newMod =
                        { mod | info = updateModuleInfo address value mod.info }

                    newDevice =
                        { device | modules = Dict.insert moduleId newMod device.modules }
                in
                updateDeviceModuleAddress deviceId moduleId (address + 1) rest (Dict.insert deviceId newDevice dict)

    else
        dict


updateDevices : Message -> Dict Int Device -> Dict Int Device
updateDevices msg dict =
    case msg.message of
        Just message ->
            case parseSxTopic msg.topic of
                Just ( deviceId, moduleId, address ) ->
                    if moduleId == 0x01 && address == 0x01 then
                        updateDeviceModules deviceId message dict

                    else
                        updateDeviceModuleAddress deviceId moduleId address message dict

                Nothing ->
                    dict

        Nothing ->
            dict


processReceivedWSMessage : List Message -> Dict Int Device -> String -> Result Decode.Error ( List Message, Dict Int Device )
processReceivedWSMessage messages devices wsmsg =
    case decodeString messageDecoder wsmsg of
        Ok decodedMessage ->
            let
                newMessage =
                    convertMessage (String.fromInt <| List.length messages) decodedMessage

                newMessages =
                    messages ++ [ newMessage ]
            in
            Ok ( newMessages, updateDevices newMessage devices )

        Err error ->
            Err error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        READY state ->
            case msg of
                GotStats result ->
                    case result of
                        Ok stats ->
                            ( READY { state | stats = stats }, Cmd.none )

                        Err error ->
                            ( ERROR_NET error, Cmd.none )

                Tick t ->
                    ( READY { state | time = t }, getStats )

                Write ->
                    let
                        message =
                            fromHex state.inputData
                                |> Maybe.andThen (\x -> Just <| B64Encode.encode <| B64Encode.bytes <| encodeListToBytes x)
                    in
                    case message of
                        Just b64message ->
                            ( model, postPub ("sx/" ++ state.inputDevice ++ "/i/w/" ++ state.inputModule ++ "/" ++ state.inputAddress ++ "/") b64message )

                        Nothing ->
                            ( model, Cmd.none )

                Read ->
                    let
                        message =
                            B64Encode.encode <| B64Encode.string <| fromChar <| fromCode 10
                    in
                    ( model, postPub ("sx/" ++ state.inputDevice ++ "/i/r/" ++ state.inputModule ++ "/" ++ state.inputAddress ++ "/") message )

                InputDevice inputDevice ->
                    ( READY { state | inputDevice = inputDevice }, Cmd.none )

                InputModule inputModule ->
                    ( READY { state | inputModule = inputModule }, Cmd.none )

                InputAddress inputAddress ->
                    ( READY { state | inputAddress = inputAddress }, Cmd.none )

                InputData inputData ->
                    ( READY { state | inputData = inputData }, Cmd.none )

                CmdSent _ ->
                    ( model, Cmd.none )

                SendWSMessage wsmsg ->
                    ( model, sendWSMessage wsmsg )

                ReceiveWSMessage wsmsg ->
                    case processReceivedWSMessage state.messages state.devices wsmsg of
                        Ok ( messages, devices ) ->
                            ( READY { state | messages = messages, devices = devices }, Cmd.none )

                        Err error ->
                            ( ERROR_DECODE error, Cmd.none )

                CheckInitReady ->
                    ( model, Cmd.none )

                Zone zone ->
                    ( READY { state | zone = zone }, Cmd.none )

        INIT maybeStats maybeTime maybeZone messages devices ->
            case msg of
                GotStats result ->
                    case result of
                        Ok stats ->
                            update CheckInitReady (INIT (Just stats) maybeTime maybeZone messages devices)

                        Err error ->
                            ( ERROR_NET error, Cmd.none )

                Tick t ->
                    update CheckInitReady (INIT maybeStats (Just t) maybeZone messages devices)

                Zone z ->
                    update CheckInitReady (INIT maybeStats maybeTime (Just z) messages devices)

                ReceiveWSMessage wsmsg ->
                    case processReceivedWSMessage messages devices wsmsg of
                        Ok ( newMessages, newDevices ) ->
                            ( INIT maybeStats maybeTime maybeZone newMessages newDevices, Cmd.none )

                        Err error ->
                            ( ERROR_DECODE error, Cmd.none )

                CheckInitReady ->
                    case model of
                        INIT (Just stats) (Just time) (Just zone) _ _ ->
                            ( READY (State stats time zone messages devices "" "" "" ""), Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ERROR_NET _ ->
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


viewModuleInfo : ModuleInfo -> Html Msg
viewModuleInfo info =
    let
        mcu =
            signatureToMCU ( info.sigrow1, info.sigrow2, info.sigrow3 )

        ram =
            mcuRam mcu

        mem =
            info.stackH * 256 + info.stackL

        memPercentage =
            (mem * 100) // ram
    in
    ul []
        [ li [] [ text <| "MCU " ++ mcuName mcu ]
        , li [] [ text <| "[" ++ toHex [ info.error, info.debug1, info.debug2, info.debug3, info.debug4 ] ++ "]" ]
        , li [] [ text <| "Memfree " ++ String.fromInt memPercentage ++ "% (" ++ String.fromInt mem ++ "/" ++ String.fromInt ram ++ ")" ]
        ]


viewDeviceModule : Module -> Html Msg
viewDeviceModule deviceModule =
    let
        sModuleId =
            String.fromInt deviceModule.id

        name =
            moduleName <| moduleTypeFromId deviceModule.id
    in
    li [ onClick (InputModule sModuleId) ] [ span [] [ text name ], viewModuleInfo deviceModule.info ]


viewDeviceModules : List Module -> Html Msg
viewDeviceModules modules =
    ul [] <| List.map viewDeviceModule modules


viewDeviceInformation : ( Int, Device ) -> Html Msg
viewDeviceInformation ( _, device ) =
    let
        sDeviceId =
            String.fromInt device.id
    in
    div [ class "device" ]
        [ span [ onClick (InputDevice sDeviceId) ] [ text sDeviceId ]
        , viewDeviceModules <| Dict.values device.modules
        ]


viewDevicesInformation : Dict Int Device -> Html Msg
viewDevicesInformation devices =
    div [ id "devices" ] <|
        [ h1 [] [ text "Known SX devices" ]
        ]
            ++ List.map viewDeviceInformation (Dict.toList devices)


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



-- TODO option to show as DEC/HEX/bits?


viewMessageBody : Maybe (List Int) -> Html Msg
viewMessageBody maybeValues =
    div [ class "message-body" ]
        [ text <| maybeToHex maybeValues
        ]


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
        , div [ class "message-header" ]
            [ div [] [ text "Received" ]
            , div [] [ text "Topic" ]
            , div [] [ text "Message" ]
            ]
        , Keyed.node "div" [ class "messages-container" ] (List.map (viewMessage state.zone) state.messages)
        ]


viewInput : State -> Html Msg
viewInput state =
    div []
        [ input [ placeholder "Device", value state.inputDevice, onInput InputDevice ] []
        , input [ placeholder "Module", value state.inputModule, onInput InputModule ] []
        , input [ placeholder "Address", value state.inputAddress, onInput InputAddress ] []
        , input [ placeholder "Data", value state.inputData, onInput InputData ] []
        , button [ onClick Read ] [ text "Read" ]
        , button [ onClick Write ] [ text "Write" ]
        ]


view : Model -> Document Msg
view model =
    case model of
        INIT _ _ _ _ _ ->
            { title = "SpyBus 2000 - Booting up", body = [ div [] [ text "Hold on to your pants" ] ] }

        READY state ->
            { title = "SpyBus 2000", body = [ div [ id "root" ] [ viewConnectionStats state.time state.stats.connectionStats, viewDevicesInformation state.devices, viewMessages state ] ] }

        ERROR_NET error ->
            let
                message =
                    case error of
                        Http.BadUrl s ->
                            "Bad url (" ++ s ++ ")"

                        Http.Timeout ->
                            "Timeout"

                        Http.NetworkError ->
                            "Network error"

                        Http.BadStatus status ->
                            "Bad status (" ++ String.fromInt status ++ ")"

                        Http.BadBody body ->
                            "Bad body (" ++ body ++ ")"
            in
            { title = "SpyBus 2000 - Error", body = [ div [] [ text <| "Fatal error has occured: " ++ message ] ] }

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
