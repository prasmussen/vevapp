module Vevapp.Moment.MonthDay exposing (parser)

import Parser exposing ((|.), (|=), Parser)
import Time
import Vevapp.Moment.Parser as ParserExtra


parser : Parser Int
parser =
    let
        f n =
            if n >= 1 && n <= 31 then
                Parser.succeed n

            else
                Parser.problem ("Not a valid month day: " ++ String.fromInt n)
    in
    Parser.succeed identity
        |= Parser.oneOf
            [ Parser.backtrackable (ParserExtra.paddedInt 2)
            , Parser.backtrackable (ParserExtra.paddedInt 1)
            ]
        |> Parser.andThen f
