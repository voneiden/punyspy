port module Main exposing (main)

import Base64.Decode as B64Decode
import Base64.Encode as B64Encode
import Browser
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode as BEncode
import Char exposing (fromCode)
import Definitions exposing (Address(..), Name(..), Register(..), deviceName, mcuName, mcuRam, moduleName, moduleRegisters, moduleTypeFromId, signatureToMCU)
import Dict exposing (Dict)
import Hex exposing (fromHex, maybeToHex, toHex)
import Html exposing (Attribute, Html, button, div, h1, h3, input, li, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, disabled, id, placeholder, value)
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
-- message filtering
-- message mode (sx/sim/freeform)
-- DONT SEND STATS REQUEST DURING INIT, ONLY AFTER INIT


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



-- @DEPRECATED


type alias SourceMessage =
    { timestamp : Time.Posix
    , topic : List String
    , message : Maybe (List Int)
    }


type alias RawMessage =
    { timestamp : Int
    , topic : List String
    , message : String
    }


sourceMessageDecoder : Decoder SourceMessage
sourceMessageDecoder =
    Decode.succeed SourceMessage
        |> required "timestamp" (int |> andThen (\t -> Decode.succeed <| Time.millisToPosix t))
        |> required "topic" (list string)
        |> required "message" (string |> andThen (\m -> Decode.succeed <| b64DecodeDefault m))


sourceMessageEncoder : SourceMessage -> Json.Encode.Value
sourceMessageEncoder sourceMessage =
    Json.Encode.object
        [ ( "timestamp", Json.Encode.int <| Time.posixToMillis sourceMessage.timestamp )
        , ( "topic", Json.Encode.list Json.Encode.string sourceMessage.topic )
        , ( "message", Json.Encode.string <| "" ) -- B64Encode.encode <| B64Encode.string  sourceMessage.message
        ]


topicDecoder : Decoder (List String)
topicDecoder =
    Decode.succeed identity
        |> required "topic" (list string)


rawMessageDecoder : Decoder RawMessage
rawMessageDecoder =
    Decode.succeed RawMessage
        |> required "timestamp" int
        |> required "topic" (list string)
        |> required "message" string


convertRawMessageToMessage : String -> RawMessage -> Message
convertRawMessageToMessage id rawMessage =
    { id = id, timestamp = Time.millisToPosix rawMessage.timestamp, topic = rawMessage.topic, message = b64DecodeDefault rawMessage.message }


convertMessage : String -> SourceMessage -> Message
convertMessage id source =
    { id = id, timestamp = source.timestamp, topic = source.topic, message = source.message }


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


{-| Decode a string into maybe list ints. Returns Nothing if decode fails
-}
b64DecodeDefault : String -> Maybe (List Int)
b64DecodeDefault s =
    case B64Decode.decode B64Decode.bytes s of
        Ok bytes ->
            Bytes.Decode.decode (decodeBytesTolist (Bytes.width bytes) Bytes.Decode.unsignedInt8) bytes

        Err _ ->
            Nothing


type alias StatsResponse =
    { connectionStats : List ConnectionStat
    }


defaultStatsResponse : StatsResponse
defaultStatsResponse =
    StatsResponse []


statsResponseDecoder : Decoder StatsResponse
statsResponseDecoder =
    list connectionStatsDecoder
        |> Decode.andThen (\connectionStats -> Decode.succeed (StatsResponse connectionStats))


type alias ModuleInfo =
    { moduleId : Int

    -- Registry values
    , sigrow1 : Int
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


defaultModuleInfo : Int -> ModuleInfo
defaultModuleInfo moduleId =
    { moduleId = moduleId
    , sigrow1 = 0
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
    { twiAddress : Int
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



-- MAIN and STATE


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


type alias InputSXFields =
    { inputDevice : String
    , inputModule : String
    , inputAddress : String
    , inputData : String
    }


type alias InputFreeFields =
    { inputTopic : String
    , inputData : String
    }


{-| Keep the other field type data always along so we can restore it when mode is switched
-}
type MessageInput
    = InputSX InputSXFields InputFreeFields
    | InputFree InputFreeFields InputSXFields


type alias Flags =
    { time : Int
    , zone : Int
    }


type alias InitialState =
    { time : Time.Posix
    , zone : Time.Zone
    }


type alias State =
    { stats : StatsResponse
    , time : Time.Posix
    , zone : Time.Zone
    , messages : List Message
    , devices : Dict Int Device
    , messageInput : MessageInput
    , initialScan : InitialScan
    }


type InitialScan
    = INITIAL_SCAN_WAIT_HISTORY_END
    | INITIAL_SCAN_WAIT_STATS
    | INITIAL_SCAN_DONE


defaultState : Time.Posix -> Time.Zone -> State
defaultState time zone =
    { stats = defaultStatsResponse
    , time = time
    , zone = zone
    , messages = []
    , devices = Dict.empty
    , messageInput = InputSX (InputSXFields "" "" "" "") (InputFreeFields "" "")
    , initialScan = INITIAL_SCAN_WAIT_HISTORY_END
    }


type
    Model
    --= INIT (Maybe StatsResponse) (Maybe Time.Posix) (Maybe Time.Zone) (List Message) (Dict Int Device) Bool
    = CONNECTING InitialState
    | READY State
    | ERROR_NET Http.Error
    | ERROR_DECODE Decode.Error


type Msg
    = GotStats StatsResponse
    | Tick Time.Posix
    | Zone Time.Zone
    | Write String (Maybe String)
    | ReadSX (Maybe String) String
    | InputDevice String
    | InputModule String
    | InputAddress String
    | InputDeviceModule String String
    | InputData String
    | CmdSent (Result Http.Error ())
      --| SendWSMessage String
    | ReceiveWSMessage String
    | OpenWS ()
    | CloseWS ()
    | CheckInitReady
    | EndHistory ()
    | NoOp ()


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( CONNECTING { time = Time.millisToPosix flags.time, zone = Time.customZone flags.zone [] }, Cmd.batch [ getTime ] )



-- UPDATE


parseSxTopic : List String -> Maybe ( Int, Int, Int )
parseSxTopic topic =
    case topic of
        "sx" :: deviceId :: "o" :: moduleId :: address :: _ ->
            case ( String.toInt deviceId, String.toInt moduleId, String.toInt address ) of
                ( Just iDeviceId, Just iModuleId, Just iAddress ) ->
                    Just ( iDeviceId, iModuleId, iAddress )

                _ ->
                    Nothing

        _ ->
            Nothing


getDeviceModuleOrDefault : Int -> Device -> Module
getDeviceModuleOrDefault twiAddress device =
    case Dict.get twiAddress device.modules of
        Just mod ->
            mod

        Nothing ->
            Module twiAddress (defaultModuleInfo -1)


getDeviceOrDefault : Int -> Dict Int Device -> Device
getDeviceOrDefault deviceId dict =
    case Dict.get deviceId dict of
        Just device ->
            device

        Nothing ->
            Device deviceId Dict.empty


updateModules : List ( Int, Int ) -> Dict Int Module
updateModules moduleInfoPairs =
    Dict.fromList <|
        List.map
            (\( twiAddr, moduleId ) ->
                ( twiAddr
                , Module twiAddr (defaultModuleInfo moduleId)
                )
            )
            moduleInfoPairs


pairInts : List Int -> List ( Int, Int )
pairInts ints =
    case ints of
        a :: b :: rest ->
            ( a, b ) :: pairInts rest

        _ ->
            []


updateDeviceModules : Int -> List Int -> Dict Int Device -> Dict Int Device
updateDeviceModules deviceId moduleInfoStream dict =
    let
        device =
            getDeviceOrDefault deviceId dict
    in
    Dict.insert deviceId { device | modules = updateModules (pairInts moduleInfoStream) } dict


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


recursivelyUpdateModuleInfo : ModuleInfo -> Int -> List Int -> ModuleInfo
recursivelyUpdateModuleInfo info address values =
    case values of
        [] ->
            info

        value :: rest ->
            recursivelyUpdateModuleInfo (updateModuleInfo address value info) (address + 1) rest


updateDeviceModuleAddress : Device -> Module -> Int -> List Int -> Dict Int Device -> Dict Int Device
updateDeviceModuleAddress device mod address values dict =
    let
        newMod =
            { mod | info = recursivelyUpdateModuleInfo mod.info address values }

        newDevice =
            { device | modules = Dict.insert mod.twiAddress newMod device.modules }
    in
    Dict.insert device.id newDevice dict


updateDevices : Maybe StatsResponse -> Message -> Dict Int Device -> ( Dict Int Device, Cmd Msg )
updateDevices maybeStats msg dict =
    case msg.message of
        Just message ->
            case parseSxTopic msg.topic of
                Just ( deviceId, twiAddress, regAddress ) ->
                    if twiAddress == 0x01 && regAddress == 0x01 then
                        let
                            newDevices =
                                updateDeviceModules deviceId message dict

                            scanCmd =
                                case maybeStats of
                                    Just stats ->
                                        scanDeviceModules stats (Dict.values newDevices)

                                    Nothing ->
                                        Cmd.none
                        in
                        ( newDevices, scanCmd )

                    else
                        let
                            device =
                                getDeviceOrDefault deviceId dict

                            mod =
                                getDeviceModuleOrDefault twiAddress device
                        in
                        ( updateDeviceModuleAddress device mod regAddress message dict, Cmd.none )

                Nothing ->
                    ( dict, Cmd.none )

        Nothing ->
            ( dict, Cmd.none )


processReceivedWSMessage : Maybe StatsResponse -> List Message -> Dict Int Device -> String -> Result Decode.Error ( List Message, Dict Int Device, Cmd Msg )
processReceivedWSMessage stats messages devices wsmsg =
    case decodeString rawMessageDecoder wsmsg of
        Ok rawMessage ->
            case rawMessage.topic of
                [ "_meta", "stats", "" ] ->
                    case decodeString statsResponseDecoder rawMessage.message of
                        Ok statsResponse ->
                            Ok ( messages, devices, Task.perform GotStats (Task.succeed statsResponse) )

                        Err error ->
                            Err error

                [ "_meta", "history", "end", "" ] ->
                    Ok ( messages, devices, Task.perform EndHistory (Task.succeed ()) )

                _ ->
                    let
                        newMessage =
                            convertRawMessageToMessage (String.fromInt <| List.length messages) rawMessage

                        newMessages =
                            newMessage :: messages

                        ( updatedDevices, cmd ) =
                            updateDevices stats newMessage devices
                    in
                    Ok ( newMessages, updatedDevices, cmd )

        Err error ->
            Err error


readMessageLength : Int -> String
readMessageLength length =
    B64Encode.encode <| B64Encode.string <| fromChar <| fromCode length


toSXTopic : Int -> Int -> Int -> String
toSXTopic deviceId moduleId address =
    "sx/" ++ String.fromInt deviceId ++ "/i/r/" ++ String.fromInt moduleId ++ "/" ++ String.fromInt address ++ "/"


requestAddressRead : Int -> Int -> Int -> Int -> Cmd Msg
requestAddressRead deviceId moduleId address length =
    postPub (toSXTopic deviceId moduleId address) (readMessageLength length)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        CONNECTING initialState ->
            case msg of
                OpenWS _ ->
                    ( READY <| defaultState initialState.time initialState.zone, Cmd.none )

                Tick t ->
                    ( CONNECTING { initialState | time = t }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        READY state ->
            case msg of
                OpenWS _ ->
                    ( READY <| defaultState state.time state.zone, Cmd.none )

                CloseWS () ->
                    ( CONNECTING { time = state.time, zone = state.zone }, Cmd.none )

                GotStats stats ->
                    case state.initialScan of
                        INITIAL_SCAN_WAIT_STATS ->
                            ( READY { state | stats = stats, initialScan = INITIAL_SCAN_DONE }, scanDeviceModules stats (Dict.values state.devices) )

                        _ ->
                            ( READY { state | stats = stats }, Cmd.none )

                Tick t ->
                    ( READY { state | time = t }, getStats )

                Write topic message ->
                    case message of
                        Just b64message ->
                            ( model, postPub topic b64message )

                        Nothing ->
                            ( model, Cmd.none )

                ReadSX maybeTopic message ->
                    case maybeTopic of
                        Just topic ->
                            ( model, postPub topic message )

                        Nothing ->
                            ( model, Cmd.none )

                InputDevice inputDevice ->
                    case state.messageInput of
                        InputSX fields other ->
                            ( READY { state | messageInput = InputSX { fields | inputDevice = inputDevice } other }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                InputModule inputModule ->
                    case state.messageInput of
                        InputSX fields other ->
                            ( READY { state | messageInput = InputSX { fields | inputModule = inputModule } other }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                InputDeviceModule inputDevice inputModule ->
                    case state.messageInput of
                        InputSX fields other ->
                            ( READY { state | messageInput = InputSX { fields | inputDevice = inputDevice, inputModule = inputModule } other }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                InputAddress inputAddress ->
                    case state.messageInput of
                        InputSX fields other ->
                            ( READY { state | messageInput = InputSX { fields | inputAddress = inputAddress } other }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                InputData inputData ->
                    case state.messageInput of
                        InputSX fields other ->
                            ( READY { state | messageInput = InputSX { fields | inputData = inputData } other }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                CmdSent _ ->
                    ( model, Cmd.none )

                --SendWSMessage wsmsg ->
                --    ( model, sendWSMessage wsmsg )
                ReceiveWSMessage wsmsg ->
                    let
                        -- Provide stats if we have received history
                        stats =
                            case state.initialScan of
                                INITIAL_SCAN_DONE ->
                                    Just state.stats

                                _ ->
                                    Nothing
                    in
                    case processReceivedWSMessage stats state.messages state.devices wsmsg of
                        Ok ( messages, devices, cmd ) ->
                            ( READY { state | messages = messages, devices = devices }, cmd )

                        Err error ->
                            ( ERROR_DECODE error, Cmd.none )

                CheckInitReady ->
                    ( model, Cmd.none )

                Zone zone ->
                    ( READY { state | zone = zone }, Cmd.none )

                NoOp () ->
                    ( model, Cmd.none )

                EndHistory () ->
                    ( READY { state | initialScan = INITIAL_SCAN_WAIT_STATS }, Cmd.none )

        ERROR_NET _ ->
            ( model, Cmd.none )

        ERROR_DECODE _ ->
            ( model, Cmd.none )



-- VIEWS


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


viewDeviceModule : String -> Module -> Html Msg
viewDeviceModule selectedModule deviceModule =
    let
        sModuleId =
            String.fromInt deviceModule.twiAddress

        name =
            moduleName <| moduleTypeFromId deviceModule.info.moduleId

        registers =
            if selectedModule == sModuleId then
                viewModuleRegisters deviceModule.info

            else
                text ""
    in
    li []
        [ span [ class "link", onClick (InputModule sModuleId) ] [ text name ]
        , viewModuleInfo deviceModule.info
        , registers
        ]


viewModuleRegisters : ModuleInfo -> Html Msg
viewModuleRegisters moduleInfo =
    ul [] [ ul [] <| List.map viewModuleRegister <| moduleRegisters <| moduleTypeFromId moduleInfo.moduleId ]


viewModuleRegister : Register -> Html Msg
viewModuleRegister (Register (Address address) size direction (Name name)) =
    li [ class "link", onClick (InputAddress (String.fromInt address)) ] [ text name ]


viewDeviceModules : String -> List Module -> Html Msg
viewDeviceModules selectedModule modules =
    ul [] <| List.map (viewDeviceModule selectedModule) modules


isSockAddrDeviceId : String -> Int -> Bool
isSockAddrDeviceId sockAddr deviceId =
    case String.split ":" sockAddr of
        [ ip, rest ] ->
            case String.split "." ip of
                [ _, _, _, id ] ->
                    id == String.fromInt deviceId

                _ ->
                    False

        _ ->
            False


isConnectedDevice : Int -> StatsResponse -> Bool
isConnectedDevice deviceId stats =
    List.length (List.filter (\s -> isSockAddrDeviceId s.sockAddr deviceId) stats.connectionStats) > 0


viewDeviceInformation : State -> ( Int, Device ) -> Html Msg
viewDeviceInformation state ( _, device ) =
    let
        sDeviceId =
            String.fromInt device.id

        offlineText =
            if isConnectedDevice device.id state.stats then
                ""

            else
                " [OFFLINE]"

        modules =
            case state.messageInput of
                InputSX fields _ ->
                    if fields.inputDevice == sDeviceId then
                        viewDeviceModules fields.inputModule <| Dict.values device.modules

                    else
                        text ""

                _ ->
                    text ""
    in
    div [ class "device" ]
        [ h3 [ class "link", onClick (InputDevice sDeviceId) ] [ text <| deviceName device.id ++ " (" ++ sDeviceId ++ ")" ++ offlineText ]
        , modules
        ]


viewDevicesInformation : State -> Html Msg
viewDevicesInformation state =
    div [ id "devices" ] <|
        [ h1 [] [ text "Known SX devices" ]
        ]
            ++ List.map (viewDeviceInformation state) (Dict.toList state.devices)


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
        , Keyed.node "div" [ class "messages-container" ] (List.map (viewMessage state.zone) (List.take 100 state.messages))
        ]


viewInput : State -> Html Msg
viewInput state =
    let
        inputs =
            case state.messageInput of
                InputSX fields _ ->
                    let
                        writeTopic =
                            "sx/" ++ fields.inputDevice ++ "/i/w/" ++ fields.inputModule ++ "/" ++ fields.inputAddress ++ "/"

                        writeMessage =
                            fromHex fields.inputData |> Maybe.andThen (\x -> Just <| B64Encode.encode <| B64Encode.bytes <| encodeListToBytes x)

                        readTopic =
                            case ( String.toInt fields.inputDevice, String.toInt fields.inputModule, String.toInt fields.inputAddress ) of
                                ( Just deviceId, Just moduleId, Just address ) ->
                                    Just <| toSXTopic deviceId moduleId address

                                _ ->
                                    Nothing

                        readMessage =
                            readMessageLength 1
                    in
                    [ input [ placeholder "Device", value fields.inputDevice, onInput InputDevice ] []
                    , input [ placeholder "Module", value fields.inputModule, onInput InputModule ] []
                    , input [ placeholder "Address", value fields.inputAddress, onInput InputAddress ] []
                    , input [ placeholder "Data", value fields.inputData, onInput InputData ] []
                    , button [ onClick (ReadSX readTopic readMessage), disabled (readTopic == Nothing) ] [ text "Read" ]
                    , button [ onClick (Write writeTopic writeMessage), disabled (writeMessage == Nothing) ] [ text "Write" ]
                    ]

                InputFree fields _ ->
                    [ input [ placeholder "Topic", value fields.inputTopic, onInput InputDevice ] []
                    , input [ placeholder "Data", value fields.inputData, onInput InputData ] []
                    ]
    in
    div [] inputs


view : Model -> Document Msg
view model =
    case model of
        CONNECTING _ ->
            { title = "SpyBus 2000 - Booting up", body = [ div [] [ text "Hold on to your pants" ] ] }

        READY state ->
            { title = "SpyBus 2000", body = [ div [ id "root" ] [ viewConnectionStats state.time state.stats.connectionStats, viewDevicesInformation state, viewMessages state ] ] }

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



-- PORTS


port sendWSMessage : Json.Encode.Value -> Cmd msg


port receiveWSMessage : (String -> msg) -> Sub msg


port openWS : (() -> msg) -> Sub msg


port closeWS : (() -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ Time.every 1000 Tick
        , receiveWSMessage ReceiveWSMessage
        , openWS OpenWS
        , closeWS CloseWS
        ]



-- COMMANDS


getStats : Cmd Msg
getStats =
    --Http.get
    --    { url = "http://localhost:18080/stats"
    --    , expect = Http.expectJson GotStats statsResponseDecoder
    --    }
    sendWSMessage <| sourceMessageEncoder (SourceMessage (Time.millisToPosix 0) [ "_meta", "stats", "" ] Nothing)


postPub : String -> String -> Cmd Msg
postPub pubTopic pubMessage =
    Http.post
        { url = "http://localhost:18080/cmd"
        , body = Http.jsonBody (pubEncode pubTopic pubMessage)
        , expect = Http.expectWhatever CmdSent
        }


pubEncode : String -> String -> Json.Encode.Value
pubEncode pubTopic pubMessage =
    Json.Encode.object
        [ ( "pubTopic", Json.Encode.string pubTopic )
        , ( "pubMessage", Json.Encode.string pubMessage )
        ]


getTime : Cmd Msg
getTime =
    Task.perform Tick Time.now


getZone : Cmd Msg
getZone =
    Task.perform Zone Time.here


scanDevice : StatsResponse -> Device -> Cmd Msg
scanDevice stats device =
    -- 246 = VREG_SIGROW_1
    -- isConnectedDevice device.id state
    if isConnectedDevice device.id stats then
        Dict.values device.modules
            |> List.map (\m -> requestAddressRead device.id m.twiAddress 246 9)
            |> Cmd.batch

    else
        Cmd.none


{-| Scans device modules for runtime information (memory, errors, mcu, debug..)
-}
scanDeviceModules : StatsResponse -> List Device -> Cmd Msg
scanDeviceModules stats devices =
    Cmd.batch <| List.map (\d -> scanDevice stats d) devices
