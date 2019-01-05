module Vevapp.Texts exposing (Text(..), toElement, toString)

import Element exposing (Element)


type Text
    = DictionaryT
    | QueryT
    | PrefixT
    | SuffixT
    | RegexT
    | QueryTypeT
    | FromLanguageT
    | ToLanguageT


toString : Text -> String
toString text =
    let
        translations =
            case text of
                DictionaryT ->
                    { en_us = "Dictionary"
                    }

                QueryT ->
                    { en_us = "Query"
                    }

                PrefixT ->
                    { en_us = "Prefix"
                    }

                SuffixT ->
                    { en_us = "Suffix"
                    }

                RegexT ->
                    { en_us = "Regex"
                    }

                QueryTypeT ->
                    { en_us = "Query type"
                    }

                FromLanguageT ->
                    { en_us = "From language"
                    }

                ToLanguageT ->
                    { en_us = "To language"
                    }
    in
    translations.en_us


toElement : Text -> Element msg
toElement text =
    text
        |> toString
        |> Element.text
