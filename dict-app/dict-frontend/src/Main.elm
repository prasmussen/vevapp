module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Cons
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attributes
import Http
import RemoteData
import Url
import Vevapp.Api.Lookup as Api
import Vevapp.Command as Command
import Vevapp.Dictionary as Dictionary
import Vevapp.Entry as Entry
import Vevapp.Language as Language
import Vevapp.LanguageDictionaryMapping as LangDictMap
import Vevapp.Query as Query
import Vevapp.QueryType as QueryType
import Vevapp.Texts as Texts


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }



-- TODO: Only have one field with langDictMap and dictionary to avoid manually syncing their state


type alias Model =
    { queryType : QueryType.QueryType
    , queryString : String
    , langDictMap : LangDictMap.LanguageDictionaryMapping
    , dictionary : Dictionary.Dictionary
    , entries : RemoteData.WebData (List Entry.Entry)
    , navKey : Nav.Key
    }


type alias QueryString =
    String


type Msg
    = SetQueryType QueryType.QueryType
    | SetQueryString String
    | SetLangDictMap LangDictMap.LanguageDictionaryMapping
    | SetDictionary Dictionary.Dictionary
    | GotEntries QueryString (RemoteData.WebData (List Entry.Entry))
    | ClickedLink Browser.UrlRequest
    | UrlChange Url.Url


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        defaultDict =
            Dictionary.NO_UK

        model =
            { queryType = QueryType.Prefix
            , queryString = ""
            , langDictMap = LangDictMap.fromDictionary defaultDict
            , dictionary = defaultDict
            , entries = RemoteData.NotAsked
            , navKey = navKey
            }

        modelWithQuery query =
            let
                dictionary =
                    Maybe.withDefault model.dictionary query.dictionary

                langDictMap =
                    LangDictMap.fromDictionary dictionary
            in
            { model
                | queryType =
                    Maybe.withDefault model.queryType query.queryType
                , queryString =
                    Maybe.withDefault model.queryString query.queryString
                , dictionary = dictionary
                , langDictMap = langDictMap
            }
    in
    case Query.parseQuery url of
        Just query ->
            ( modelWithQuery query, Cmd.none )
                |> Command.chain getEntries

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQueryType queryType ->
            ( { model | queryType = queryType }, Cmd.none )
                |> Command.chain getEntries
                |> Command.chain updateUrl

        SetQueryString queryString ->
            if String.isEmpty queryString then
                ( { model | queryString = "", entries = RemoteData.NotAsked }, Cmd.none )
                    |> Command.chain updateUrl

            else
                ( { model | queryString = queryString }, Cmd.none )
                    |> Command.chain getEntries
                    |> Command.chain updateUrl

        SetLangDictMap langDictMap ->
            let
                dictionary =
                    LangDictMap.toDictionary langDictMap
            in
            ( { model | langDictMap = langDictMap, dictionary = dictionary }, Cmd.none )
                |> Command.chain getEntries
                |> Command.chain updateUrl

        SetDictionary dictionary ->
            let
                langDictMap =
                    LangDictMap.fromDictionary dictionary
            in
            ( { model | langDictMap = langDictMap, dictionary = dictionary }, Cmd.none )
                |> Command.chain getEntries
                |> Command.chain updateUrl

        GotEntries queryString webData ->
            if queryString /= model.queryString then
                ( model, Cmd.none )

            else
                ( { model | entries = webData }, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.load (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChange url ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = Texts.toString Texts.DictionaryT
    , body = [ content model ]
    }


content : Model -> Html Msg
content model =
    let
        rows =
            [ Element.row [ Element.width Element.fill ] [ fromLanguageToggle model ]
            , Element.row [ Element.width Element.fill ] [ toLanguageToggle model ]
            , Element.row [ Element.width Element.fill ] [ queryTypeToggle model ]
            , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ queryInput model ]
            , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ entriesSection model ]
            ]

        column =
            Element.column
                [ Element.width (Element.maximum 1000 Element.fill)
                , Element.spacing 20
                , Element.padding 20
                , Element.centerX
                , Font.size 14
                , Background.color (Element.rgb255 245 247 250)
                ]
                rows
    in
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color (Element.rgb255 245 247 250)
        ]
        column


queryInput : Model -> Element Msg
queryInput model =
    let
        label =
            Input.labelAbove [ Font.bold, Font.size 14 ] (Texts.toElement Texts.QueryT)
    in
    Input.text
        [ borderColor
        , Font.size 20
        , Element.spacing 10
        , Input.focusedOnLoad
        ]
        { onChange = SetQueryString
        , text = model.queryString
        , placeholder = Nothing
        , label = label
        }


queryTypeToggle : Model -> Element Msg
queryTypeToggle model =
    let
        optionAttributes : QueryType.QueryType -> List (Element.Attribute Msg)
        optionAttributes queryType =
            let
                isSelected =
                    queryType == model.queryType

                borderAttr =
                    if queryType == QueryType.Prefix then
                        Border.widthEach
                            { bottom = 1
                            , right = 1
                            , left = 1
                            , top = 1
                            }

                    else
                        Border.widthEach
                            { bottom = 1
                            , right = 1
                            , left = 0
                            , top = 1
                            }

                sharedAttrs =
                    [ Border.color (Element.rgb255 217 217 217)
                    , borderAttr
                    , Element.width Element.fill
                    , Element.padding 13
                    , Font.size 18
                    , Element.pointer
                    , Events.onClick (SetQueryType queryType)
                    ]
            in
            if isSelected then
                sharedAttrs
                    ++ [ Background.color (Element.rgba255 70 70 70 0.1)
                       , Font.color (Element.rgb255 0 0 0)
                       ]

            else
                sharedAttrs
                    ++ [ Background.color (Element.rgb255 255 255 255)
                       , Font.color (Element.rgb255 0 0 0)
                       ]

        prefixOption =
            Element.el
                (optionAttributes QueryType.Prefix)
                (Element.el [ Element.centerX, Element.centerY ] (Texts.toElement Texts.PrefixT))

        suffixOption =
            Element.el
                (optionAttributes QueryType.Suffix)
                (Element.el [ Element.centerX, Element.centerY ] (Texts.toElement Texts.SuffixT))

        regexOption =
            Element.el
                (optionAttributes QueryType.Regex)
                (Element.el [ Element.centerX, Element.centerY ] (Texts.toElement Texts.RegexT))
    in
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el [ Font.bold ] (Texts.toElement Texts.QueryTypeT)
        , Element.row [ Element.width Element.fill, Element.spacing 0 ] [ prefixOption, suffixOption, regexOption ]
        ]


fromLanguageToggle : Model -> Element Msg
fromLanguageToggle model =
    let
        optionAttributes : Int -> LangDictMap.LanguageDictionaryMapping -> List (Element.Attribute Msg)
        optionAttributes index langDictMap =
            let
                isSelected =
                    langDictMap == model.langDictMap

                borderAttr =
                    if index == 0 then
                        Border.widthEach
                            { bottom = 1
                            , right = 1
                            , left = 1
                            , top = 1
                            }

                    else
                        Border.widthEach
                            { bottom = 1
                            , right = 1
                            , left = 0
                            , top = 1
                            }

                sharedAttrs =
                    [ Border.color (Element.rgb255 217 217 217)
                    , borderAttr
                    , Element.width Element.fill
                    , Element.padding 13
                    , Font.size 18
                    , Element.pointer
                    , Events.onClick (SetLangDictMap langDictMap)
                    ]
            in
            if isSelected then
                sharedAttrs
                    ++ [ Background.color (Element.rgba255 70 70 70 0.1)
                       , Font.color (Element.rgb255 0 0 0)
                       ]

            else
                sharedAttrs
                    ++ [ Background.color (Element.rgb255 255 255 255)
                       , Font.color (Element.rgb255 0 0 0)
                       ]

        options =
            List.indexedMap toOption LangDictMap.all

        toOption index langDictMap =
            let
                languageName =
                    Language.toFlag langDictMap.from
            in
            Element.el
                (optionAttributes index langDictMap)
                (Element.el [ Element.centerX, Element.centerY ] (Element.text languageName))
    in
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el [ Font.bold ] (Texts.toElement Texts.FromLanguageT)
        , Element.row [ Element.width Element.fill, Element.spacing 0 ] options
        ]


toLanguageToggle : Model -> Element Msg
toLanguageToggle model =
    let
        optionAttributes : Int -> Dictionary.Dictionary -> List (Element.Attribute Msg)
        optionAttributes index dictionary =
            let
                isSelected =
                    dictionary == model.dictionary

                borderAttr =
                    if index == 0 then
                        Border.widthEach
                            { bottom = 1
                            , right = 1
                            , left = 1
                            , top = 1
                            }

                    else
                        Border.widthEach
                            { bottom = 1
                            , right = 1
                            , left = 0
                            , top = 1
                            }

                sharedAttrs =
                    [ Border.color (Element.rgb255 217 217 217)
                    , borderAttr
                    , Element.width Element.fill
                    , Element.padding 13
                    , Font.size 18
                    , Element.pointer
                    , Events.onClick (SetDictionary dictionary)
                    ]
            in
            if isSelected then
                sharedAttrs
                    ++ [ Background.color (Element.rgba255 70 70 70 0.1)
                       , Font.color (Element.rgb255 0 0 0)
                       ]

            else
                sharedAttrs
                    ++ [ Background.color (Element.rgb255 255 255 255)
                       , Font.color (Element.rgb255 0 0 0)
                       ]

        options =
            List.indexedMap toOption (Cons.toList model.langDictMap.to)

        toOption index dictionary =
            let
                languageName =
                    Dictionary.toLanguagePair dictionary
                        |> Tuple.second
                        |> Language.toFlag
            in
            Element.el
                (optionAttributes index dictionary)
                (Element.el [ Element.centerX, Element.centerY ] (Element.text languageName))
    in
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el [ Font.bold ] (Texts.toElement Texts.ToLanguageT)
        , Element.row [ Element.width Element.fill, Element.spacing 0 ] options
        ]


entriesSection : Model -> Element Msg
entriesSection model =
    case model.entries of
        RemoteData.NotAsked ->
            Element.none

        RemoteData.Loading ->
            Element.none

        RemoteData.Failure error ->
            case error of
                Http.BadUrl url ->
                    Element.text ("Invalid url was requested: " ++ url)

                Http.Timeout ->
                    Element.text "Request timed out"

                Http.NetworkError ->
                    Element.text "Network error"

                Http.BadStatus code ->
                    Element.text ("Api responded with status code " ++ String.fromInt code)

                Http.BadBody err ->
                    Element.text ("Failed to decode response body: " ++ err)

        RemoteData.Success entries ->
            let
                toEntry entry =
                    Element.column [ Element.width Element.fill, Element.spacing 10 ]
                        [ Element.el
                            [ Font.size 26
                            , Font.light
                            , Font.color (Element.rgb255 34 35 36)
                            , fontFamily
                            ]
                            (Element.text entry.word)
                        , Element.column [ Element.spacing 10 ] (List.map toTranslation entry.translations)
                        ]

                toTranslation trans =
                    Element.paragraph
                        [ Font.size 16
                        , Font.light
                        , Font.color (Element.rgb255 34 35 36)
                        , fontFamily
                        ]
                        [ Element.text trans ]
            in
            Element.column [ Element.width Element.fill, Element.spacing 30 ] (List.map toEntry entries)


fontFamily =
    Font.family
        [ Font.typeface "Helvetica Neue"
        , Font.typeface "Helvetica"
        , Font.typeface "Arial"
        , Font.sansSerif
        ]


borderColor : Element.Attribute Msg
borderColor =
    Border.color (Element.rgb255 211 214 219)


getEntries : Model -> Cmd Msg
getEntries model =
    if String.isEmpty model.queryString then
        Cmd.none

    else
        Api.lookup (GotEntries model.queryString) model


updateUrl : Model -> Cmd Msg
updateUrl model =
    Nav.replaceUrl model.navKey (Query.buildQuery model)
