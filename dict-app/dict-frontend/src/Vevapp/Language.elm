module Vevapp.Language exposing
    ( Language(..)
    , toFlag
    , toString
    )


type Language
    = English
    | Norwegian
    | Swedish
    | Deutsch
    | French
    | Spanish
    | NorwegianMedical


toString : Language -> String
toString language =
    case language of
        English ->
            "English"

        Norwegian ->
            "Norwegian"

        Swedish ->
            "Swedish"

        Deutsch ->
            "Deutsch"

        French ->
            "French"

        Spanish ->
            "Spanish"

        NorwegianMedical ->
            "Medical"


toFlag : Language -> String
toFlag language =
    case language of
        English ->
            "🇬🇧"

        Norwegian ->
            "🇳🇴"

        Swedish ->
            "🇸🇪"

        Deutsch ->
            "🇩🇪"

        French ->
            "🇲🇫"

        Spanish ->
            "🇪🇸"

        NorwegianMedical ->
            "🇳🇴 ⚕"
