module Main exposing (main)

import Browser
import Browser.Extra as BrowserExtra
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Iso8601
import Json.Decode as Decode
import Random
import Task exposing (Task)
import Time
import Url
import Vevapp.Moment as Moment exposing (Moment)
import Vevapp.Port as Port
import Vevapp.Reminder as Reminder exposing (Reminder)
import Vevapp.User as User exposing (User)


main =
    BrowserExtra.elementWithTask
        { init = init
        , loadingView = loadingView
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Maybe Model -> Sub Msg
subscriptions _ =
    Port.receive FromJavascript FromJavascriptError


type alias Model =
    { time : Time.Posix
    , title : String
    , when : String
    }


type Msg
    = SetTitle String
    | SetWhen String
    | Init
    | FromJavascript Port.MessageFromJavascript
    | FromJavascriptError String


init : Maybe User -> ( Task Never Model, Cmd Msg )
init maybeUser =
    let
        task =
            Task.map toModel Time.now

        toModel time =
            { time = time
            , title = ""
            , when = ""
            }

        cmd =
            Task.perform (\_ -> Init) (Task.succeed ())
    in
    ( task, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTitle title ->
            ( { model | title = title }, Cmd.none )

        SetWhen when ->
            let
                _ =
                    Moment.parse when
                        |> Debug.log "moment"
            in
            ( { model | when = when }, Cmd.none )

        Init ->
            let
                options =
                    listRemindersOptions model.time

                cmd =
                    Port.send (Port.ListReminders options)
            in
            ( model, cmd )

        FromJavascript jsMsg ->
            updateFromJavascript jsMsg model

        FromJavascriptError err ->
            let
                _ =
                    Debug.log "FromJavascriptError" err
            in
            ( model, Cmd.none )


updateFromJavascript : Port.MessageFromJavascript -> Model -> ( Model, Cmd Msg )
updateFromJavascript msg model =
    case msg of
        Port.ListRemindersSuccess reminders ->
            ( model, Cmd.none )

        Port.ListRemindersFailure err ->
            ( model, Cmd.none )


listRemindersOptions : Time.Posix -> Reminder.ListOptions
listRemindersOptions minTime =
    let
        maxTime =
            minTime
                |> Time.posixToMillis
                |> (\ms -> ms + 365 * 24 * 60 * 60 * 1000)
                |> Time.millisToPosix
    in
    { calendarId = "primary"
    , maxResults = 100
    , timeMin = Iso8601.fromTime minTime
    , timeMax = Iso8601.fromTime maxTime
    , orderBy = "startTime"
    , singleEvents = True
    , privateExtendedProperty = "isReminder=true"
    }


loadingView : Html Msg
loadingView =
    Html.text ""


view : Model -> Html Msg
view model =
    let
        rows =
            [ Element.row [ Element.width Element.fill, Element.spacing 20 ] [ titleInput model ]
            , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ whenInput model ]
            ]

        column =
            Element.column
                [ Element.width (Element.maximum 800 Element.fill)
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


titleInput : Model -> Element Msg
titleInput model =
    let
        label =
            Input.labelAbove [ Font.bold, Font.size 14 ] (Element.text "Title")
    in
    Input.text
        [ borderColor
        , Font.size 20
        , Element.spacing 10
        ]
        { onChange = SetTitle
        , text = model.title
        , placeholder = Nothing
        , label = label
        }


whenInput : Model -> Element Msg
whenInput model =
    let
        label =
            Input.labelAbove [ Font.bold, Font.size 14 ] (Element.text "When")
    in
    Input.text
        [ borderColor
        , Font.size 20
        , Element.spacing 10
        ]
        { onChange = SetWhen
        , text = model.when
        , placeholder = Nothing
        , label = label
        }


borderColor : Element.Attribute Msg
borderColor =
    Border.color (Element.rgb255 211 214 219)
