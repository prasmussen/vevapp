module Vevapp.Moment.Month exposing
    ( numberParser
    , toString
    , wordParser
    )

import Parser exposing ((|.), (|=), Parser)
import Time
import Vevapp.Moment.Parser as ParserExtra


wordParser : Parser Time.Month
wordParser =
    Parser.oneOf
        [ januaryParser
        , februaryParser
        , marchParser
        , aprilParser
        , mayParser
        , juneParser
        , julyParser
        , augustParser
        , septemberParser
        , octoberParser
        , novemberParser
        , decemberParser
        ]


toString : Time.Month -> String
toString month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


numberParser : Parser Time.Month
numberParser =
    let
        f n =
            case monthFromInt n of
                Just month ->
                    Parser.succeed month

                Nothing ->
                    Parser.problem ("Not a valid month: " ++ String.fromInt n)
    in
    Parser.succeed identity
        |= Parser.oneOf
            [ Parser.backtrackable (ParserExtra.paddedInt 2)
            , Parser.backtrackable (ParserExtra.paddedInt 1)
            ]
        |> Parser.andThen f


monthFromInt : Int -> Maybe Time.Month
monthFromInt n =
    case n of
        1 ->
            Just Time.Jan

        2 ->
            Just Time.Feb

        3 ->
            Just Time.Mar

        4 ->
            Just Time.Apr

        5 ->
            Just Time.May

        6 ->
            Just Time.Jun

        7 ->
            Just Time.Jul

        8 ->
            Just Time.Aug

        9 ->
            Just Time.Sep

        10 ->
            Just Time.Oct

        11 ->
            Just Time.Nov

        12 ->
            Just Time.Dec

        _ ->
            Nothing


januaryParser : Parser Time.Month
januaryParser =
    Parser.succeed Time.Jan
        |. Parser.oneOf
            [ Parser.symbol "january"
            , Parser.symbol "jan"
            ]


februaryParser : Parser Time.Month
februaryParser =
    Parser.succeed Time.Feb
        |. Parser.oneOf
            [ Parser.symbol "february"
            , Parser.symbol "feb"
            ]


marchParser : Parser Time.Month
marchParser =
    Parser.succeed Time.Mar
        |. Parser.oneOf
            [ Parser.symbol "march"
            , Parser.symbol "mar"
            ]


aprilParser : Parser Time.Month
aprilParser =
    Parser.succeed Time.Apr
        |. Parser.oneOf
            [ Parser.symbol "april"
            , Parser.symbol "apr"
            ]


mayParser : Parser Time.Month
mayParser =
    Parser.succeed Time.May
        |. Parser.oneOf
            [ Parser.symbol "may"
            ]


juneParser : Parser Time.Month
juneParser =
    Parser.succeed Time.Jun
        |. Parser.oneOf
            [ Parser.symbol "june"
            ]


julyParser : Parser Time.Month
julyParser =
    Parser.succeed Time.Jul
        |. Parser.oneOf
            [ Parser.symbol "july"
            ]


augustParser : Parser Time.Month
augustParser =
    Parser.succeed Time.Aug
        |. Parser.oneOf
            [ Parser.symbol "august"
            , Parser.symbol "aug"
            ]


septemberParser : Parser Time.Month
septemberParser =
    Parser.succeed Time.Sep
        |. Parser.oneOf
            [ Parser.symbol "september"
            , Parser.symbol "sept"
            , Parser.symbol "sep"
            ]


octoberParser : Parser Time.Month
octoberParser =
    Parser.succeed Time.Oct
        |. Parser.oneOf
            [ Parser.symbol "october"
            , Parser.symbol "oct"
            ]


novemberParser : Parser Time.Month
novemberParser =
    Parser.succeed Time.Nov
        |. Parser.oneOf
            [ Parser.symbol "november"
            , Parser.symbol "nov"
            ]


decemberParser : Parser Time.Month
decemberParser =
    Parser.succeed Time.Dec
        |. Parser.oneOf
            [ Parser.symbol "december"
            , Parser.symbol "dec"
            ]
