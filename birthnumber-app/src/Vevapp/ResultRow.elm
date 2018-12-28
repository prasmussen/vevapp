module Vevapp.ResultRow exposing (ResultRow, fromViewModel)

import Date
import Vevapp.BirthNumber as BirthNumber
import Vevapp.Texts as Texts
import Vevapp.ViewModel as ViewModel


type alias ResultRow =
    { key : Texts.Text
    , value : String
    }


fromViewModel : ViewModel.ValidModel -> List ResultRow
fromViewModel model =
    let
        birthNumber =
            model.birthNumber

        currentDate =
            model.currentDate

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

        birthDate =
            BirthNumber.getBirthday birthNumber

        birthday =
            Date.format "d MMMM y" birthDate

        age =
            Date.diff Date.Years birthDate currentDate
                |> abs
                |> String.fromInt
    in
    [ { key = Texts.BirthNumberT
      , value = BirthNumber.toString birthNumber
      }
    , { key = Texts.BirthdayT
      , value = birthday
      }
    , { key = Texts.AgeT
      , value = age
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
