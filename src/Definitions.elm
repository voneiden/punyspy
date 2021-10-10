module Definitions exposing (..)


type Address
    = Address Int


type Size
    = Size Int


type Direction
    = RO
    | WO
    | RW


type Register
    = Register Address Size Direction Name


type Name
    = Name String


type Module
    = MOD_ETH
    | MOD_MAX7219
    | MOD_IO
    | MOD_UNKNOWN


type MCU
    = MCU_ATTINY814
    | MCU_ATTINY414
    | MCU_UNKNOWN


mcuName : MCU -> String
mcuName mcu =
    case mcu of
        MCU_ATTINY814 ->
            "ATtiny814"

        MCU_ATTINY414 ->
            "ATtiny414"

        MCU_UNKNOWN ->
            "Unknown"


signatureToMCU : ( Int, Int, Int ) -> MCU
signatureToMCU signature =
    case signature of
        ( 0x1E, 0x93, 0x22 ) ->
            MCU_ATTINY814

        ( 0x1E, 0x92, 0x22 ) ->
            MCU_ATTINY414

        _ ->
            MCU_UNKNOWN


mcuRam : MCU -> Int
mcuRam mcu =
    case mcu of
        MCU_ATTINY814 ->
            512

        MCU_ATTINY414 ->
            256

        MCU_UNKNOWN ->
            0


deviceName : Int -> String
deviceName id =
    case id of
        48 ->
            "MCDU, Captain"

        _ ->
            "Unknown device"


moduleName : Module -> String
moduleName id =
    case id of
        MOD_ETH ->
            "Ethernet Gateway"

        MOD_MAX7219 ->
            "MAX7219"

        MOD_IO ->
            "IO"

        MOD_UNKNOWN ->
            "Unknown module"


moduleTypeFromId : Int -> Module
moduleTypeFromId id =
    case id of
        0x01 ->
            MOD_ETH

        0x0A ->
            MOD_MAX7219

        0x0B ->
            MOD_IO

        _ ->
            MOD_UNKNOWN


type DeviceId
    = DeviceId Int


type Device
    = Device DeviceId (List Module)


moduleRegisters : Module -> List Register
moduleRegisters moduleType =
    case moduleType of
        MOD_ETH ->
            [ Register (Address 0x01) (Size 1) RO (Name "Device modules")
            ]

        MOD_MAX7219 ->
            [ Register (Address 0x00) (Size 1) WO (Name "NOP")
            , Register (Address 0x01) (Size 1) WO (Name "D0")
            , Register (Address 0x02) (Size 1) WO (Name "D1")
            , Register (Address 0x03) (Size 1) WO (Name "D2")
            , Register (Address 0x04) (Size 1) WO (Name "D3")
            , Register (Address 0x05) (Size 1) WO (Name "D4")
            , Register (Address 0x06) (Size 1) WO (Name "D5")
            , Register (Address 0x07) (Size 1) WO (Name "D6")
            , Register (Address 0x08) (Size 1) WO (Name "D7")
            , Register (Address 0x09) (Size 1) WO (Name "Decode mode")
            , Register (Address 0x0A) (Size 1) WO (Name "Intensity")
            , Register (Address 0x0B) (Size 1) WO (Name "Scan limit")
            , Register (Address 0x0C) (Size 1) WO (Name "Display On")
            , Register (Address 0x0F) (Size 1) WO (Name "Display Test")
            , Register (Address 0xA0) (Size 8) WO (Name "ATOI")
            , Register (Address 0xFB) (Size 1) RW (Name "Last error")
            , Register (Address 0xFC) (Size 1) RW (Name "Debug 1")
            , Register (Address 0xFD) (Size 1) RW (Name "Debug 2")
            , Register (Address 0xFE) (Size 1) RW (Name "Debug 3")
            , Register (Address 0xFF) (Size 1) RW (Name "Debug 4")
            ]

        MOD_IO ->
            []

        MOD_UNKNOWN ->
            []
