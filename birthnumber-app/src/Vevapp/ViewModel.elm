module Vevapp.ViewModel exposing
    ( ViewModel(..)
    , new
    )

import Date
import Vevapp.BirthNumber as BirthNumber
import Vevapp.Texts as Texts


type ViewModel
    = ParseFailure BirthNumber.ParseFailure
    | InvalidNineDigitBirthNumber
    | WrongCheckDigits BirthNumber.ParsedBirthNumber
    | WrongBirthDate BirthNumber.ParsedBirthNumber
    | ValidBirthNumber BirthNumber.BirthNumber


type alias PartialModel a =
    { a
        | birthNumber : String
        , currentDate : Date.Date
    }


new : PartialModel a -> ViewModel
new model =
    let
        validate parsedBirthNumber =
            case BirthNumber.validate model.currentDate parsedBirthNumber of
                Err BirthNumber.MissingCheckDigits ->
                    let
                        maybeCheckDigits =
                            BirthNumber.calcCheckDigits parsedBirthNumber
                    in
                    case maybeCheckDigits of
                        Nothing ->
                            InvalidNineDigitBirthNumber

                        Just checkDigits ->
                            validate (BirthNumber.setCheckDigits checkDigits parsedBirthNumber)

                Err BirthNumber.WrongCheckDigits ->
                    WrongCheckDigits parsedBirthNumber

                Err BirthNumber.WrongBirthdate ->
                    WrongBirthDate parsedBirthNumber

                Ok birthNumber ->
                    ValidBirthNumber birthNumber
    in
    case BirthNumber.parse model.birthNumber of
        Err failure ->
            ParseFailure failure

        Ok parsedBirthNumber ->
            validate parsedBirthNumber
