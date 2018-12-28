module Vevapp.BirthNumber exposing
    ( BirthNumber
    , CheckDigits
    , Gender(..)
    , IndividualNumber
    , ParseFailure(..)
    , ParsedBirthNumber
    , ValidationFailure(..)
    , calcCheckDigits
    , getBirthday
    , getCheckDigits
    , getGender
    , getIndividualNumber
    , parse
    , setCheckDigits
    , toString
    , validate
    )

import Date
import Maybe.Extra as MaybeE


type ParsedBirthNumber
    = ParsedBirthNumber
        { birthDate : BirthDate
        , personNumber : ParsedPersonNumber
        }


type BirthNumber
    = BirthNumber
        { birthDate : BirthDate
        , personNumber : PersonNumber
        , birthday : Date.Date
        }


type alias BirthDate =
    { year :
        { firstDigit : Int
        , secondDigit : Int
        }
    , month :
        { firstDigit : Int
        , secondDigit : Int
        }
    , day :
        { firstDigit : Int
        , secondDigit : Int
        }
    }


type alias ParsedPersonNumber =
    { individualNumber : IndividualNumber
    , checkDigits : Maybe CheckDigits
    }


type alias PersonNumber =
    { individualNumber : IndividualNumber
    , checkDigits : CheckDigits
    }


type alias CheckDigits =
    { firstDigit : Int
    , secondDigit : Int
    }


type alias IndividualNumber =
    { firstDigit : Int
    , secondDigit : Int
    , thirdDigit : Int
    }


type ParseFailure
    = InvalidInput
    | WrongInputLength


type ValidationFailure
    = MissingCheckDigits
    | WrongCheckDigits
    | WrongBirthdate


parse : String -> Result ParseFailure ParsedBirthNumber
parse input =
    let
        maybeDigits : Maybe (List Int)
        maybeDigits =
            input
                |> String.toList
                |> List.map String.fromChar
                |> List.map String.toInt
                |> MaybeE.combine
    in
    case maybeDigits of
        Just [ d1, d2, m1, m2, y1, y2, i1, i2, i3, c1, c2 ] ->
            Ok <|
                ParsedBirthNumber
                    { birthDate =
                        { year =
                            { firstDigit = y1
                            , secondDigit = y2
                            }
                        , month =
                            { firstDigit = m1
                            , secondDigit = m2
                            }
                        , day =
                            { firstDigit = d1
                            , secondDigit = d2
                            }
                        }
                    , personNumber =
                        { individualNumber =
                            { firstDigit = i1
                            , secondDigit = i2
                            , thirdDigit = i3
                            }
                        , checkDigits =
                            Just
                                { firstDigit = c1
                                , secondDigit = c2
                                }
                        }
                    }

        Just [ d1, d2, m1, m2, y1, y2, i1, i2, i3 ] ->
            Ok <|
                ParsedBirthNumber
                    { birthDate =
                        { year =
                            { firstDigit = y1
                            , secondDigit = y2
                            }
                        , month =
                            { firstDigit = m1
                            , secondDigit = m2
                            }
                        , day =
                            { firstDigit = d1
                            , secondDigit = d2
                            }
                        }
                    , personNumber =
                        { individualNumber =
                            { firstDigit = i1
                            , secondDigit = i2
                            , thirdDigit = i3
                            }
                        , checkDigits = Nothing
                        }
                    }

        Just _ ->
            Err WrongInputLength

        Nothing ->
            Err InvalidInput


validate : Date.Date -> ParsedBirthNumber -> Result ValidationFailure BirthNumber
validate currentDay ((ParsedBirthNumber bnr) as birthNumber) =
    let
        toValidated : Date.Date -> CheckDigits -> BirthNumber
        toValidated birthday checkDigits =
            BirthNumber
                { birthDate = bnr.birthDate
                , personNumber =
                    { individualNumber = bnr.personNumber.individualNumber
                    , checkDigits = checkDigits
                    }
                , birthday = birthday
                }
    in
    case getBirthDate currentDay birthNumber of
        Nothing ->
            Err WrongBirthdate

        Just date ->
            case bnr.personNumber.checkDigits of
                Nothing ->
                    Err MissingCheckDigits

                Just checkDigits ->
                    if not (validCheckDigits birthNumber) then
                        Err WrongCheckDigits

                    else
                        Ok (toValidated date checkDigits)


validCheckDigits : ParsedBirthNumber -> Bool
validCheckDigits ((ParsedBirthNumber bnr) as birthNumber) =
    case calcCheckDigits birthNumber of
        Nothing ->
            False

        Just checkDigits ->
            bnr.personNumber.checkDigits == Just checkDigits


toString : BirthNumber -> String
toString (BirthNumber birthNumber) =
    let
        day =
            birthNumber.birthDate.day

        month =
            birthNumber.birthDate.month

        year =
            birthNumber.birthDate.year

        indNumber =
            birthNumber.personNumber.individualNumber

        checkDigits =
            birthNumber.personNumber.checkDigits
    in
    [ day.firstDigit
    , day.secondDigit
    , month.firstDigit
    , month.secondDigit
    , year.firstDigit
    , year.secondDigit
    , indNumber.firstDigit
    , indNumber.secondDigit
    , indNumber.thirdDigit
    , checkDigits.firstDigit
    , checkDigits.secondDigit
    ]
        |> List.map String.fromInt
        |> String.concat


getIndividualNumber : BirthNumber -> IndividualNumber
getIndividualNumber (BirthNumber bnr) =
    bnr.personNumber.individualNumber


getCheckDigits : BirthNumber -> CheckDigits
getCheckDigits (BirthNumber bnr) =
    bnr.personNumber.checkDigits


setCheckDigits : CheckDigits -> ParsedBirthNumber -> ParsedBirthNumber
setCheckDigits checkDigits (ParsedBirthNumber bnr) =
    ParsedBirthNumber
        { birthDate = bnr.birthDate
        , personNumber =
            { individualNumber = bnr.personNumber.individualNumber
            , checkDigits = Just checkDigits
            }
        }


calcCheckDigits : ParsedBirthNumber -> Maybe CheckDigits
calcCheckDigits (ParsedBirthNumber birthNumber) =
    let
        day =
            birthNumber.birthDate.day

        month =
            birthNumber.birthDate.month

        year =
            birthNumber.birthDate.year

        indNumber =
            birthNumber.personNumber.individualNumber

        calcDigit : List Int -> Int
        calcDigit digits =
            List.foldl (+) 0 digits
                |> mod11

        mod11 digit =
            case modBy 11 digit of
                0 ->
                    0

                n ->
                    11 - n

        firstCheckDigit =
            calcDigit
                [ 3 * day.firstDigit
                , 7 * day.secondDigit
                , 6 * month.firstDigit
                , 1 * month.secondDigit
                , 8 * year.firstDigit
                , 9 * year.secondDigit
                , 4 * indNumber.firstDigit
                , 5 * indNumber.secondDigit
                , 2 * indNumber.thirdDigit
                ]

        secondCheckDigit =
            calcDigit
                [ 5 * day.firstDigit
                , 4 * day.secondDigit
                , 3 * month.firstDigit
                , 2 * month.secondDigit
                , 7 * year.firstDigit
                , 6 * year.secondDigit
                , 5 * indNumber.firstDigit
                , 4 * indNumber.secondDigit
                , 3 * indNumber.thirdDigit
                , 2 * firstCheckDigit
                ]
    in
    if firstCheckDigit >= 10 || secondCheckDigit >= 10 then
        Nothing

    else
        Just
            { firstDigit = firstCheckDigit
            , secondDigit = secondCheckDigit
            }


getBirthday : BirthNumber -> Date.Date
getBirthday (BirthNumber bnr) =
    bnr.birthday



-- TODO: find a better way to construct a date


getBirthDate : Date.Date -> ParsedBirthNumber -> Maybe Date.Date
getBirthDate currentDate (ParsedBirthNumber birthNumber) =
    let
        currentYear =
            Date.year currentDate

        day =
            birthNumber.birthDate.day

        month =
            birthNumber.birthDate.month

        year =
            birthNumber.birthDate.year

        fullDay =
            (day.firstDigit * 10) + day.secondDigit

        fullMonth =
            (month.firstDigit * 10) + month.secondDigit

        partialYear =
            (year.firstDigit * 10) + year.secondDigit

        fullYear =
            if 2000 + partialYear <= currentYear then
                2000 + partialYear

            else
                1900 + partialYear

        isoString =
            String.join "-"
                [ String.padLeft 4 '0' <| String.fromInt fullYear
                , String.padLeft 2 '0' <| String.fromInt fullMonth
                , String.padLeft 2 '0' <| String.fromInt fullDay
                ]
    in
    Date.fromIsoString isoString
        |> Result.toMaybe


type Gender
    = Male
    | Female


getGender : BirthNumber -> Gender
getGender (BirthNumber bnr) =
    let
        isEven n =
            modBy 2 n == 0
    in
    if isEven bnr.personNumber.individualNumber.thirdDigit then
        Female

    else
        Male
