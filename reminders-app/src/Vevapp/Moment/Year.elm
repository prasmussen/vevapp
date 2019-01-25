module Vevapp.Moment.Year exposing (parser)

import Parser exposing ((|.), (|=), Parser)
import Time


parser : Parser Int
parser =
    let
        f year =
            if year > 2018 && year < 3000 then
                Parser.succeed year

            else
                Parser.problem ("Not a supported year: " ++ String.fromInt year)
    in
    Parser.succeed identity
        |= Parser.int
        |> Parser.andThen f


paddedInt : Int -> Parser Int
paddedInt quantity =
    let
        f str =
            if String.length str == quantity then
                -- FYI: String.toInt works on zero-padded integers
                case String.toInt str of
                    Just intVal ->
                        Parser.succeed intVal

                    Nothing ->
                        Parser.problem ("Invalid integer: \"" ++ str ++ "\"")

            else
                Parser.problem
                    ("Expected "
                        ++ String.fromInt quantity
                        ++ " digits, but got "
                        ++ String.fromInt (String.length str)
                    )
    in
    Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen f
