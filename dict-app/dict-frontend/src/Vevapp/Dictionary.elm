module Vevapp.Dictionary exposing
    ( Dictionary(..)
    , fromString
    , toLanguagePair
    , toString
    )

import Vevapp.Language as Language


type Dictionary
    = NO_UK
    | NO_NO
    | NO_DE
    | NO_ME
    | UK_NO
    | UK_UK
    | UK_FR
    | UK_SE
    | UK_ES
    | SE_UK
    | DE_UK
    | FR_UK
    | ES_UK


toString : Dictionary -> String
toString dictionary =
    case dictionary of
        NO_UK ->
            "no_uk"

        NO_NO ->
            "no_no"

        NO_DE ->
            "no_de"

        NO_ME ->
            "no_me"

        UK_NO ->
            "uk_no"

        UK_UK ->
            "uk_uk"

        UK_FR ->
            "uk_fr"

        UK_SE ->
            "uk_se"

        UK_ES ->
            "uk_es"

        SE_UK ->
            "se_uk"

        DE_UK ->
            "de_uk"

        FR_UK ->
            "fr_uk"

        ES_UK ->
            "es_uk"


fromString : String -> Maybe Dictionary
fromString str =
    case str of
        "no_uk" ->
            Just NO_UK

        "no_no" ->
            Just NO_NO

        "no_de" ->
            Just NO_DE

        "no_me" ->
            Just NO_ME

        "uk_no" ->
            Just UK_NO

        "uk_uk" ->
            Just UK_UK

        "uk_fr" ->
            Just UK_FR

        "uk_se" ->
            Just UK_SE

        "uk_es" ->
            Just UK_ES

        "se_uk" ->
            Just SE_UK

        "de_uk" ->
            Just DE_UK

        "fr_uk" ->
            Just FR_UK

        "es_uk" ->
            Just ES_UK

        _ ->
            Nothing


toLanguagePair : Dictionary -> ( Language.Language, Language.Language )
toLanguagePair dictionary =
    case dictionary of
        NO_UK ->
            ( Language.Norwegian, Language.English )

        NO_NO ->
            ( Language.Norwegian, Language.Norwegian )

        NO_DE ->
            ( Language.Norwegian, Language.Deutsch )

        NO_ME ->
            ( Language.Norwegian, Language.NorwegianMedical )

        UK_NO ->
            ( Language.English, Language.Norwegian )

        UK_UK ->
            ( Language.English, Language.English )

        UK_FR ->
            ( Language.English, Language.French )

        UK_SE ->
            ( Language.English, Language.Swedish )

        UK_ES ->
            ( Language.English, Language.Spanish )

        SE_UK ->
            ( Language.Swedish, Language.English )

        DE_UK ->
            ( Language.Deutsch, Language.English )

        FR_UK ->
            ( Language.French, Language.English )

        ES_UK ->
            ( Language.Spanish, Language.English )
