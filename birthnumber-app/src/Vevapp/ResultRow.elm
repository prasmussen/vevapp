module Vevapp.ResultRow exposing (ResultRow, fromBirthNumber)

import Date
import Vevapp.BirthNumber as BirthNumber
import Vevapp.Texts as Texts
import Vevapp.ViewModel as ViewModel


type alias ResultRow =
    { key : Texts.Text
    , value : String
    }


fromBirthNumber : BirthNumber.BirthNumber -> List ResultRow
fromBirthNumber birthNumber =
    let
        checkDigits =
            let
                digits =
                    BirthNumber.getCheckDigits birthNumber
            in
            String.concat
                [ String.fromInt digits.firstDigit
                , String.fromInt digits.secondDigit
                ]

        individualNumber =
            let
                digits =
                    BirthNumber.getIndividualNumber birthNumber
            in
            String.concat
                [ String.fromInt digits.firstDigit
                , String.fromInt digits.secondDigit
                , String.fromInt digits.thirdDigit
                ]

        gender =
            case BirthNumber.getGender birthNumber of
                BirthNumber.Male ->
                    "Male"

                BirthNumber.Female ->
                    "Female"

        birthday =
            Date.format "d MMMM y" (BirthNumber.getBirthday birthNumber)
    in
    [ { key = Texts.BirthNumberT
      , value = BirthNumber.toString birthNumber
      }
    , { key = Texts.BirthdayT
      , value = birthday
      }
    , { key = Texts.AgeT
      , value = "todo"
      }
    , { key = Texts.GenderT
      , value = gender
      }
    , { key = Texts.IndividualNumberT
      , value = individualNumber
      }
    , { key = Texts.CheckDigitsT
      , value = checkDigits
      }
    ]
