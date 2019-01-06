module Vevapp.LanguageDictionaryMapping exposing
    ( LanguageDictionaryMapping
    , all
    , fromDictionary
    , toDictionary
    )

import Cons exposing (Cons)
import Vevapp.Dictionary as Dictionary
import Vevapp.Language as Language


type alias LanguageDictionaryMapping =
    { from : Language.Language
    , to : Cons Dictionary.Dictionary
    }


all : List LanguageDictionaryMapping
all =
    [ fromNorwegianMapping
    , fromEnglishMapping
    , fromSwedishMapping
    , fromDeutschMapping
    , fromFrenchMapping
    , fromSpanishMapping
    ]


fromDictionary : Dictionary.Dictionary -> LanguageDictionaryMapping
fromDictionary dict =
    case dict of
        Dictionary.NO_UK ->
            fromNorwegianMapping

        Dictionary.NO_NO ->
            fromNorwegianMapping

        Dictionary.NO_DE ->
            fromNorwegianMapping

        Dictionary.NO_ME ->
            fromNorwegianMapping

        Dictionary.UK_NO ->
            fromEnglishMapping

        Dictionary.UK_UK ->
            fromEnglishMapping

        Dictionary.UK_FR ->
            fromEnglishMapping

        Dictionary.UK_SE ->
            fromEnglishMapping

        Dictionary.UK_ES ->
            fromEnglishMapping

        Dictionary.SE_UK ->
            fromSwedishMapping

        Dictionary.DE_UK ->
            fromDeutschMapping

        Dictionary.FR_UK ->
            fromFrenchMapping

        Dictionary.ES_UK ->
            fromSpanishMapping


toDictionary : LanguageDictionaryMapping -> Dictionary.Dictionary
toDictionary langDictMap =
    Cons.head langDictMap.to


fromNorwegianMapping : LanguageDictionaryMapping
fromNorwegianMapping =
    { from = Language.Norwegian
    , to =
        Cons.cons Dictionary.NO_UK
            [ Dictionary.NO_NO
            , Dictionary.NO_DE
            , Dictionary.NO_ME
            ]
    }


fromEnglishMapping : LanguageDictionaryMapping
fromEnglishMapping =
    { from = Language.English
    , to =
        Cons.cons Dictionary.UK_NO
            [ Dictionary.UK_UK
            , Dictionary.UK_FR
            , Dictionary.UK_SE
            , Dictionary.UK_ES
            ]
    }


fromSwedishMapping : LanguageDictionaryMapping
fromSwedishMapping =
    { from = Language.Swedish
    , to = Cons.singleton Dictionary.SE_UK
    }


fromDeutschMapping : LanguageDictionaryMapping
fromDeutschMapping =
    { from = Language.Deutsch
    , to = Cons.singleton Dictionary.DE_UK
    }


fromFrenchMapping : LanguageDictionaryMapping
fromFrenchMapping =
    { from = Language.French
    , to = Cons.singleton Dictionary.FR_UK
    }


fromSpanishMapping : LanguageDictionaryMapping
fromSpanishMapping =
    { from = Language.Spanish
    , to = Cons.singleton Dictionary.ES_UK
    }
