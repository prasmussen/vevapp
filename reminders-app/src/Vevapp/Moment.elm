module Vevapp.Moment exposing
    ( Moment(..)
    , parse
    , toTime
    )

import Derberos.Date.Core as Derberos
import Derberos.Date.Delta as Derberos
import Derberos.Date.Utils as Derberos
import Parser exposing ((|.), (|=), Parser)
import Time
import Vevapp.Moment.Clock as Clock exposing (Clock)
import Vevapp.Moment.Month as Month
import Vevapp.Moment.MonthDay as MonthDay
import Vevapp.Moment.Parser as ParserExtra
import Vevapp.Moment.TimeUnit as TimeUnit exposing (TimeUnit)
import Vevapp.Moment.Weekday as Weekday
import Vevapp.Moment.Year as Year


type Moment
    = At Clock
    | In Int TimeUnit
    | On Time.Weekday (Maybe Clock)
    | Month Time.Month Int (Maybe Clock)
    | Date Int Time.Month Int (Maybe Clock)
    | Next Time.Weekday (Maybe Clock)
    | Tomorrow (Maybe Clock)


toTime : Time.Posix -> Time.Zone -> Moment -> Time.Posix
toTime now zone moment =
    case moment of
        At clock ->
            setClock zone now clock
                |> ensureFuture 1 now

        In value unit ->
            case unit of
                TimeUnit.Minute ->
                    Derberos.addMinutes value now

                TimeUnit.Hour ->
                    Derberos.addHours value now

                TimeUnit.Day ->
                    Derberos.addDays value now

                TimeUnit.Week ->
                    Derberos.addDays (value * 7) now

                TimeUnit.Month ->
                    Derberos.addMonths value zone now

                TimeUnit.Year ->
                    Derberos.addYears value now

        On weekday maybeClock ->
            let
                time =
                    Derberos.nextWeekdayFromTime weekday zone now
            in
            case maybeClock of
                Just clock ->
                    setClock zone time clock
                        |> ensureFuture 7 now

                Nothing ->
                    time

        Month month day maybeClock ->
            let
                currentCivil =
                    now
                        |> Derberos.addTimezoneMilliseconds zone
                        |> Derberos.posixToCivil

                newCivil =
                    { currentCivil
                        | month = Derberos.monthToNumber1 month
                        , day = day
                        , second = 0
                        , zone = zone
                    }

                ensureFutureYear civil =
                    case comparePosix (Derberos.civilToPosix civil) now of
                        LT ->
                            { civil | year = civil.year + 1 }

                        _ ->
                            civil
            in
            case maybeClock of
                Just clock ->
                    { newCivil | hour = clock.hour, minute = clock.minute }
                        |> ensureFutureYear
                        |> Derberos.civilToPosix

                Nothing ->
                    newCivil
                        |> ensureFutureYear
                        |> Derberos.civilToPosix

        Date year month day maybeClock ->
            let
                currentCivil =
                    now
                        |> Derberos.addTimezoneMilliseconds zone
                        |> Derberos.posixToCivil

                newCivil =
                    { currentCivil
                        | year = year
                        , month = Derberos.monthToNumber1 month
                        , day = day
                        , second = 0
                        , zone = zone
                    }
            in
            case maybeClock of
                Just clock ->
                    { newCivil | hour = clock.hour, minute = clock.minute }
                        |> Derberos.civilToPosix

                Nothing ->
                    newCivil
                        |> Derberos.civilToPosix

        Next weekday maybeClock ->
            let
                time =
                    Derberos.nextWeekdayFromTime weekday zone now
            in
            case maybeClock of
                Just clock ->
                    setClock zone time clock
                        |> ensureFuture 7 now

                Nothing ->
                    time

        Tomorrow maybeClock ->
            let
                time =
                    Derberos.addDays 1 now
            in
            case maybeClock of
                Just clock ->
                    setClock zone time clock

                Nothing ->
                    time


comparePosix : Time.Posix -> Time.Posix -> Order
comparePosix a b =
    compare (Time.posixToMillis a) (Time.posixToMillis b)


monthsTo : Time.Month -> Time.Month -> Int
monthsTo from to =
    let
        diff =
            Derberos.monthToNumber1 to - Derberos.monthToNumber1 from
    in
    if diff < 0 then
        abs diff + 12

    else
        diff


ensureFuture : Int -> Time.Posix -> Time.Posix -> Time.Posix
ensureFuture days now future =
    if Time.posixToMillis now > Time.posixToMillis future then
        Derberos.addDays days future

    else
        future


setClock : Time.Zone -> Time.Posix -> Clock -> Time.Posix
setClock zone time clock =
    time
        |> Derberos.resetTime
        |> Derberos.adjustMilliseconds zone
        |> Derberos.addHours clock.hour
        |> Derberos.addMinutes clock.minute


parse : String -> Maybe Moment
parse input =
    input
        |> String.trim
        |> String.toLower
        |> ParserExtra.try
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
    Parser.succeed (\day month year -> Date year month day Nothing)
        |= Parser.int
        |. Parser.spaces
        |= Month.wordParser
        |. Parser.spaces
        |= Year.parser
        |. Parser.end


ddmmyyyyParser : Parser Moment
ddmmyyyyParser =
    Parser.succeed (\day month year -> Date year month day Nothing)
        |= MonthDay.parser
        |. Parser.symbol "."
        |= Month.numberParser
        |. Parser.symbol "."
        |= Year.parser
        |. Parser.end


ddmmyyyyClockParser : Parser Moment
ddmmyyyyClockParser =
    Parser.succeed (\day month year clock -> Date year month day (Just clock))
        |= MonthDay.parser
        |. Parser.symbol "."
        |= Month.numberParser
        |. Parser.symbol "."
        |= Year.parser
        |. Parser.spaces
        |= Clock.parser
        |. Parser.end
