module Vevapp.Texts exposing (Text(..), toElement, toString)

import Element exposing (Element)


type Text
    = BirthNumberValidatorT
    | NorwegianBirthNumberT
    | BirthNumberT
    | ResultT
    | CheckDigitsT
    | GenderT
    | BirthdayT
    | IndividualNumberT
    | AgeT


toString : Text -> String
toString text =
    let
        translations =
            case text of
                BirthNumberValidatorT ->
                    { en_us = "Birth number validator"
                    }

                NorwegianBirthNumberT ->
                    { en_us = "Norwegian birth number"
                    }

                BirthNumberT ->
                    { en_us = "Birth number"
                    }

                ResultT ->
                    { en_us = "Result"
                    }

                CheckDigitsT ->
                    { en_us = "Check digits"
                    }

                GenderT ->
                    { en_us = "Gender"
                    }

                BirthdayT ->
                    { en_us = "Birthday"
                    }

                IndividualNumberT ->
                    { en_us = "Individual number"
                    }

                AgeT ->
                    { en_us = "Age"
                    }
    in
    translations.en_us


toElement : Text -> Element msg
toElement text =
    text
        |> toString
        |> Element.text
