module Vevapp.Dictionary exposing
    ( Dictionary(..)
    , Language(..)
    , LanguageDictPair
    , dictFromLanguageDictPair
    , dictToLanguageDictPair
    , fromString
    , languageDictPairs
    , languageToFlag
    , languageToString
    , toLanguagePair
    , toString
    )

import Cons exposing (Cons)


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


type Language
    = English
    | Norwegian
    | Swedish
    | Deutsch
    | French
    | Spanish
    | NorwegianMedical



-- TODO: move to Language.elm


languageToString : Language -> String
languageToString language =
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


languageToFlag : Language -> String
languageToFlag language =
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


toLanguagePair : Dictionary -> ( Language, Language )
toLanguagePair dictionary =
    case dictionary of
        NO_UK ->
            ( Norwegian, English )

        NO_NO ->
            ( Norwegian, Norwegian )

        NO_DE ->
            ( Norwegian, Deutsch )

        NO_ME ->
            ( Norwegian, NorwegianMedical )

        UK_NO ->
            ( English, Norwegian )

        UK_UK ->
            ( English, English )

        UK_FR ->
            ( English, French )

        UK_SE ->
            ( English, Swedish )

        UK_ES ->
            ( English, Spanish )

        SE_UK ->
            ( Swedish, English )

        DE_UK ->
            ( Deutsch, English )

        FR_UK ->
            ( French, English )

        ES_UK ->
            ( Spanish, English )


type alias LanguageDictPair =
    { from : Language
    , to : Cons Dictionary
    }


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


dictToLanguageDictPair : Dictionary -> LanguageDictPair
dictToLanguageDictPair dict =
    case dict of
        NO_UK ->
            fromNorwegian

        NO_NO ->
            fromNorwegian

        NO_DE ->
            fromNorwegian

        NO_ME ->
            fromNorwegian

        UK_NO ->
            fromEnglish

        UK_UK ->
            fromEnglish

        UK_FR ->
            fromEnglish

        UK_SE ->
            fromEnglish

        UK_ES ->
            fromEnglish

        SE_UK ->
            fromSwedish

        DE_UK ->
            fromDeutsch

        FR_UK ->
            fromFrench

        ES_UK ->
            fromSpanish


dictFromLanguageDictPair : LanguageDictPair -> Dictionary
dictFromLanguageDictPair languageDictPair =
    Cons.head languageDictPair.to


fromNorwegian : LanguageDictPair
fromNorwegian =
    { from = Norwegian
    , to = Cons.cons NO_UK [ NO_NO, NO_DE, NO_ME ]
    }


fromEnglish : LanguageDictPair
fromEnglish =
    { from = English
    , to = Cons.cons UK_NO [ UK_UK, UK_FR, UK_SE, UK_ES ]
    }


fromSwedish : LanguageDictPair
fromSwedish =
    { from = Swedish
    , to = Cons.singleton SE_UK
    }


fromDeutsch : LanguageDictPair
fromDeutsch =
    { from = Deutsch
    , to = Cons.singleton DE_UK
    }


fromFrench : LanguageDictPair
fromFrench =
    { from = French
    , to = Cons.singleton FR_UK
    }


fromSpanish : LanguageDictPair
fromSpanish =
    { from = Spanish
    , to = Cons.singleton ES_UK
    }


languageDictPairs : List LanguageDictPair
languageDictPairs =
    [ fromNorwegian
    , fromEnglish
    , fromSwedish
    , fromDeutsch
    , fromFrench
    , fromSpanish
    ]
