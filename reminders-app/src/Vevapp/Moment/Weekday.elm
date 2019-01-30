module Vevapp.Moment.Weekday exposing
    ( parser
    , toString
    )

import Parser exposing ((|.), (|=), Parser)
import Time


parser : Parser Time.Weekday
parser =
    Parser.oneOf
        [ mondayParser
        , tuesdayParser
        , wednesdayParser
        , thursdayParser
        , fridayParser
        , saturdayParser
        , sundayParser
        ]


toString : Time.Weekday -> String
toString weekday =
    case weekday of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"


mondayParser : Parser Time.Weekday
mondayParser =
    Parser.succeed Time.Mon
        |. Parser.oneOf
            [ Parser.symbol "monday"
            ]


tuesdayParser : Parser Time.Weekday
tuesdayParser =
    Parser.succeed Time.Tue
        |. Parser.oneOf
            [ Parser.symbol "tuesday"
            ]


wednesdayParser : Parser Time.Weekday
wednesdayParser =
    Parser.succeed Time.Wed
        |. Parser.oneOf
            [ Parser.symbol "wednesday"
            ]


thursdayParser : Parser Time.Weekday
thursdayParser =
    Parser.succeed Time.Thu
        |. Parser.oneOf
            [ Parser.symbol "thursday"
            ]


fridayParser : Parser Time.Weekday
fridayParser =
    Parser.succeed Time.Fri
        |. Parser.oneOf
            [ Parser.symbol "friday"
            ]


saturdayParser : Parser Time.Weekday
saturdayParser =
    Parser.succeed Time.Sat
        |. Parser.oneOf
            [ Parser.symbol "saturday"
            ]


sundayParser : Parser Time.Weekday
sundayParser =
    Parser.succeed Time.Sun
        |. Parser.oneOf
            [ Parser.symbol "sunday"
            ]
