module Vevapp.Moment exposing
    ( Moment(..)
    , parse
    )

import Parser exposing ((|.), (|=), Parser)
import Time
import Vevapp.Moment.Clock as Clock exposing (Clock)
import Vevapp.Moment.Month as Month
import Vevapp.Moment.MonthDay as MonthDay
import Vevapp.Moment.TimeUnit as TimeUnit exposing (TimeUnit)
import Vevapp.Moment.Weekday as Weekday
import Vevapp.Moment.Year as Year


type Moment
    = At Clock
    | In Int TimeUnit
    | On Time.Weekday (Maybe Clock)
    | Month Time.Month Int (Maybe Clock)
    | Date Int Time.Month Int
    | DateTime Int Time.Month Int Clock
    | Next Time.Weekday (Maybe Clock)
    | Tomorrow (Maybe Clock)
    | Unknown String


parse : String -> Moment
parse input =
    let
        parsers =
            [ valueUnitParser
            , inParser
            , fromNowParser
            , onDayClockParser
            , dayClockParser
            , clockDayParser
            , onDayParser
            , nextDayClockParser
            , clockNextDayParser
            , nextDayParser
            , tomorrowClockParser
            , clockTomorrowParser
            , tomorrowParser
            , clockParser
            , dayMonthParser
            , dayMonthClockParser
            , monthDayParser
            , clockMonthDayParser
            , dayMonthYearParser
            , ddmmyyyyParser
            , ddmmyyyyClockParser
            ]

        preparedInput =
            input
                |> String.trim
                |> String.toLower
    in
    case try parsers preparedInput of
        Just res ->
            res

        Nothing ->
            Unknown preparedInput


try : List (Parser a) -> String -> Maybe a
try parsers input =
    case parsers of
        parser :: rest ->
            case Parser.run parser input of
                Ok res ->
                    Just res

                Err _ ->
                    try rest input

        [] ->
            Nothing


tomorrowParser : Parser Moment
tomorrowParser =
    Parser.succeed (Tomorrow Nothing)
        |. Parser.symbol "tomorrow"
        |. Parser.end


tomorrowClockParser : Parser Moment
tomorrowClockParser =
    Parser.succeed (\clock -> Tomorrow (Just clock))
        |. Parser.symbol "tomorrow"
        |. Parser.spaces
        |= Clock.parser
        |. Parser.end


clockTomorrowParser : Parser Moment
clockTomorrowParser =
    Parser.succeed (\clock -> Tomorrow (Just clock))
        |= Clock.parser
        |. Parser.spaces
        |. Parser.symbol "tomorrow"
        |. Parser.end


clockParser : Parser Moment
clockParser =
    Parser.succeed At
        |= Clock.parser
        |. Parser.end


valueUnitParser : Parser Moment
valueUnitParser =
    Parser.succeed In
        |= Parser.int
        |. Parser.spaces
        |= TimeUnit.parser
        |. Parser.end


inParser : Parser Moment
inParser =
    Parser.succeed In
        |. Parser.symbol "in"
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= TimeUnit.parser
        |. Parser.end


fromNowParser : Parser Moment
fromNowParser =
    Parser.succeed In
        |= Parser.int
        |. Parser.spaces
        |= TimeUnit.parser
        |. Parser.spaces
        |. Parser.symbol "from"
        |. Parser.spaces
        |. Parser.symbol "now"
        |. Parser.end


onDayParser : Parser Moment
onDayParser =
    Parser.succeed (\day -> On day Nothing)
        |. Parser.symbol "on"
        |. Parser.spaces
        |= Weekday.parser
        |. Parser.end


onDayClockParser : Parser Moment
onDayClockParser =
    Parser.succeed (\day clock -> On day (Just clock))
        |. Parser.symbol "on"
        |. Parser.spaces
        |= Weekday.parser
        |. Parser.spaces
        |= Clock.parser
        |. Parser.end


dayClockParser : Parser Moment
dayClockParser =
    Parser.succeed (\day clock -> On day (Just clock))
        |. Parser.spaces
        |= Weekday.parser
        |. Parser.spaces
        |= Clock.parser
        |. Parser.end


clockDayParser : Parser Moment
clockDayParser =
    Parser.succeed (\clock day -> On day (Just clock))
        |. Parser.spaces
        |= Clock.parser
        |. Parser.spaces
        |= Weekday.parser
        |. Parser.end


nextDayParser : Parser Moment
nextDayParser =
    Parser.succeed (\day -> Next day Nothing)
        |. Parser.symbol "next"
        |. Parser.spaces
        |= Weekday.parser
        |. Parser.end


nextDayClockParser : Parser Moment
nextDayClockParser =
    Parser.succeed (\day clock -> Next day (Just clock))
        |. Parser.symbol "next"
        |. Parser.spaces
        |= Weekday.parser
        |. Parser.spaces
        |= Clock.parser
        |. Parser.end


clockNextDayParser : Parser Moment
clockNextDayParser =
    Parser.succeed (\clock day -> Next day (Just clock))
        |= Clock.parser
        |. Parser.spaces
        |. Parser.symbol "next"
        |. Parser.spaces
        |= Weekday.parser
        |. Parser.end


dayMonthParser : Parser Moment
dayMonthParser =
    Parser.succeed (\day month -> Month month day Nothing)
        |= Parser.int
        |. Parser.spaces
        |= Month.wordParser
        |. Parser.end


dayMonthClockParser : Parser Moment
dayMonthClockParser =
    Parser.succeed (\day month clock -> Month month day (Just clock))
        |= Parser.int
        |. Parser.spaces
        |= Month.wordParser
        |. Parser.spaces
        |= Clock.parser
        |. Parser.end


monthDayParser : Parser Moment
monthDayParser =
    Parser.succeed (\month day -> Month month day Nothing)
        |= Month.wordParser
        |. Parser.spaces
        |= Parser.int
        |. Parser.end


clockMonthDayParser : Parser Moment
clockMonthDayParser =
    Parser.succeed (\clock month day -> Month month day (Just clock))
        |= Clock.parser
        |. Parser.spaces
        |= Month.wordParser
        |. Parser.spaces
        |= Parser.int
        |. Parser.end


dayMonthYearParser : Parser Moment
dayMonthYearParser =
    Parser.succeed (\day month year -> Date year month day)
        |= Parser.int
        |. Parser.spaces
        |= Month.wordParser
        |. Parser.spaces
        |= Year.parser
        |. Parser.end


ddmmyyyyParser : Parser Moment
ddmmyyyyParser =
    Parser.succeed (\day month year -> Date year month day)
        |= MonthDay.parser
        |. Parser.symbol "."
        |= Month.numberParser
        |. Parser.symbol "."
        |= Year.parser
        |. Parser.end


ddmmyyyyClockParser : Parser Moment
ddmmyyyyClockParser =
    Parser.succeed (\day month year clock -> DateTime year month day clock)
        |= MonthDay.parser
        |. Parser.symbol "."
        |= Month.numberParser
        |. Parser.symbol "."
        |= Year.parser
        |. Parser.spaces
        |= Clock.parser
        |. Parser.end
