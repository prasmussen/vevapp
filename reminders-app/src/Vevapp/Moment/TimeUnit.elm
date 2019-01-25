module Vevapp.Moment.TimeUnit exposing
    ( TimeUnit(..)
    , parser
    )

import Parser exposing ((|.), (|=), Parser)
import Time


type TimeUnit
    = Minute
    | Hour
    | Day
    | Week
    | Month
    | Year


parser : Parser TimeUnit
parser =
    Parser.oneOf
        [ yearParser
        , monthParser
        , weekParser
        , dayParser
        , hourParser
        , minuteParser
        ]


minuteParser : Parser TimeUnit
minuteParser =
    Parser.succeed Minute
        |. Parser.oneOf
            [ Parser.symbol "minutes"
            , Parser.symbol "minute"
            , Parser.symbol "mins"
            , Parser.symbol "min"
            , Parser.symbol "m"
            ]


hourParser : Parser TimeUnit
hourParser =
    Parser.succeed Hour
        |. Parser.oneOf
            [ Parser.symbol "hours"
            , Parser.symbol "hour"
            , Parser.symbol "hr"
            , Parser.symbol "h"
            ]


dayParser : Parser TimeUnit
dayParser =
    Parser.succeed Day
        |. Parser.oneOf
            [ Parser.symbol "days"
            , Parser.symbol "day"
            ]


weekParser : Parser TimeUnit
weekParser =
    Parser.succeed Week
        |. Parser.oneOf
            [ Parser.symbol "weeks"
            , Parser.symbol "week"
            ]


monthParser : Parser TimeUnit
monthParser =
    Parser.succeed Month
        |. Parser.oneOf
            [ Parser.symbol "months"
            , Parser.symbol "month"
            ]


yearParser : Parser TimeUnit
yearParser =
    Parser.succeed Year
        |. Parser.oneOf
            [ Parser.symbol "years"
            , Parser.symbol "year"
            ]
