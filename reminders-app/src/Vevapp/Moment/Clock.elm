module Vevapp.Moment.Clock exposing
    ( Clock
    , parser
    )

import Parser exposing ((|.), (|=), Parser)
import Vevapp.Moment.Parser as ParserExtra


type alias Clock =
    { hour : Int
    , minute : Int
    }


parser : Parser Clock
parser =
    Parser.succeed Clock
        |= Parser.oneOf
            [ Parser.backtrackable (ParserExtra.paddedInt 2)
            , Parser.backtrackable (ParserExtra.paddedInt 1)
            ]
        |. Parser.oneOf
            [ Parser.symbol ":"
            , Parser.symbol "."
            ]
        |= Parser.oneOf
            [ Parser.backtrackable (ParserExtra.paddedInt 2)
            , Parser.backtrackable (ParserExtra.paddedInt 1)
            ]
