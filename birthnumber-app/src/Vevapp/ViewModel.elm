module Vevapp.ViewModel exposing
    ( ParsedModel
    , ValidModel
    , ViewModel(..)
    , new
    )

import Date
import Vevapp.BirthNumber as BirthNumber
import Vevapp.Texts as Texts


type alias ParsedModel =
    { birthNumber : BirthNumber.ParsedBirthNumber
    , currentDate : Date.Date
    }


type alias ValidModel =
    { birthNumber : BirthNumber.BirthNumber
    , currentDate : Date.Date
    }


type ViewModel
    = MissingCurrentDate
    | ParseFailure BirthNumber.ParseFailure
    | InvalidNineDigitBirthNumber
    | WrongCheckDigits BirthNumber.ParsedBirthNumber
    | WrongBirthDate BirthNumber.ParsedBirthNumber
    | ValidBirthNumber ValidModel


type alias PartialModel a =
    { a
        | birthNumber : String
        , currentDate : Maybe Date.Date
    }


new : PartialModel a -> ViewModel
new model =
    let
        parse currentDate =
            case BirthNumber.parse model.birthNumber of
                Err failure ->
                    ParseFailure failure

                Ok parsedBirthNumber ->
                    validate currentDate parsedBirthNumber

        validate currentDate parsedBirthNumber =
            case BirthNumber.validate currentDate parsedBirthNumber of
                Err BirthNumber.MissingCheckDigits ->
                    let
                        maybeCheckDigits =
                            BirthNumber.calcCheckDigits parsedBirthNumber
                    in
                    case maybeCheckDigits of
                        Nothing ->
                            InvalidNineDigitBirthNumber

                        Just checkDigits ->
                            validate currentDate (BirthNumber.setCheckDigits checkDigits parsedBirthNumber)

                Err BirthNumber.WrongCheckDigits ->
                    WrongCheckDigits parsedBirthNumber

                Err BirthNumber.WrongBirthdate ->
                    WrongBirthDate parsedBirthNumber

                Ok birthNumber ->
                    ValidBirthNumber
                        { birthNumber = birthNumber
                        , currentDate = currentDate
                        }
    in
    case model.currentDate of
        Nothing ->
            MissingCurrentDate

        Just currentDate ->
            parse currentDate
