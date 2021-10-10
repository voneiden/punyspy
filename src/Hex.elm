module Hex exposing (fromHex, toHex, maybeToHex)


hexify : Int -> String
hexify i =
    String.fromList [ iToHex (i // 16), iToHex (modBy 16 i) ]


toHex : List Int -> String
toHex values =
    String.join " " <| List.map hexify values

maybeToHex : Maybe (List Int) -> String
maybeToHex maybeValues =
    case maybeValues of
        Just values ->
            String.join " " <| List.map hexify values

        Nothing ->
            "(No data)"


pairToInt : ( Char, Char ) -> Maybe Int
pairToInt ( a, b ) =
    Maybe.map2 (\i1 i2 -> i1 * 16 + i2) (hexToI a) (hexToI b)


pairsToInts : List Char -> Maybe (List Int)
pairsToInts s =
    case s of
        [] ->
            Just []

        [ _ ] ->
            Nothing

        a :: b :: rest ->
            case pairToInt ( a, b ) of
                Just i ->
                    case pairsToInts rest of
                        Just restInts ->
                            Just <| i :: restInts

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing


fromHex : String -> Maybe (List Int)
fromHex s =
    pairsToInts <| String.toList <| String.replace " " "" s


iToHex : Int -> Char
iToHex i =
    case i of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'A'

        11 ->
            'B'

        12 ->
            'C'

        13 ->
            'D'

        14 ->
            'E'

        15 ->
            'F'

        _ ->
            '?'


hexToI : Char -> Maybe Int
hexToI h =
    case h of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'A' ->
            Just 10

        'B' ->
            Just 11

        'C' ->
            Just 12

        'D' ->
            Just 13

        'E' ->
            Just 14

        'F' ->
            Just 15

        _ ->
            Nothing
