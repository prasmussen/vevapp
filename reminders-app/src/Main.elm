module Main exposing (main)

import Browser
import Browser.Extra as BrowserExtra
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Iso8601
import Json.Decode as JD
import RemoteData exposing (RemoteData)
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
    , reminders : RemoteData String (List Reminder)
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
            , reminders = RemoteData.NotAsked
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
            ( { model | reminders = RemoteData.Loading }, cmd )

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
            ( { model | reminders = RemoteData.Success reminders }, Cmd.none )

        Port.ListRemindersFailure err ->
            ( { model | reminders = RemoteData.Failure err }, Cmd.none )


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
            , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ viewReminders model ]
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


viewReminders : Model -> Element Msg
viewReminders model =
    let
        rowPadding =
            Element.padding 10

        rowColor index =
            if modBy 2 index == 0 then
                Background.color (Element.rgb255 255 255 255)

            else
                Background.color (Element.rgb255 245 247 250)

        leftColumnView index reminder =
            Element.el
                [ rowColor index
                , Font.bold
                , Border.widthEach
                    { bottom = 1
                    , left = 1
                    , right = 1
                    , top = 0
                    }
                , borderColor
                , rowPadding
                ]
                (Element.text reminder.summary)

        rightColumnView index reminder =
            Element.el
                [ rowColor index
                , rowPadding
                , Border.widthEach
                    { bottom = 1
                    , left = 0
                    , right = 1
                    , top = 0
                    }
                , borderColor
                ]
                (Element.text (Iso8601.fromTime reminder.startDate))

        table =
            case model.reminders of
                RemoteData.NotAsked ->
                    Element.text "Not asked"

                RemoteData.Loading ->
                    Element.text "Loading..."

                RemoteData.Failure err ->
                    Element.text ("Failed to load reminders: " ++ err)

                RemoteData.Success reminders ->
                    Element.indexedTable
                        [ Border.widthEach
                            { bottom = 0
                            , left = 0
                            , right = 0
                            , top = 1
                            }
                        , borderColor
                        ]
                        { data = reminders
                        , columns =
                            [ { header = Element.none
                              , width = Element.fill
                              , view = leftColumnView
                              }
                            , { header = Element.none
                              , width = Element.fill
                              , view = rightColumnView
                              }
                            ]
                        }
    in
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el [ Font.bold ] (Element.text "Upcoming reminders")
        , table
        ]


borderColor : Element.Attribute Msg
borderColor =
    Border.color (Element.rgb255 211 214 219)
