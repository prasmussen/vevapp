module Vevapp.Moment.Parser exposing
    ( paddedInt
    , try
    )

import Parser exposing ((|.), (|=), Parser)


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
