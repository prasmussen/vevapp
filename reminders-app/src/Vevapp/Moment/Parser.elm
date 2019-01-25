module Vevapp.Moment.Parser exposing (paddedInt)

import Parser exposing ((|.), (|=), Parser)


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
