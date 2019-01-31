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
import Vevapp.Moment.Month as Month
import Vevapp.Moment.Weekday as Weekday
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
    , zone : Time.Zone
    , summary : String
    , when : String
    , reminders : RemoteData String (List Reminder)
    }


type Msg
    = SetSummary String
    | SetWhen String
    | Init
    | CreateReminder
    | FromJavascript Port.MessageFromJavascript
    | FromJavascriptError String


init : Maybe User -> ( Task Never Model, Cmd Msg )
init maybeUser =
    let
        task =
            Task.map2 toModel Time.now Time.here

        toModel time zone =
            { time = time
            , zone = zone
            , summary = ""
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
        SetSummary summary ->
            ( { model | summary = summary }, Cmd.none )

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

        CreateReminder ->
            ( model, Cmd.none )

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
                |> addYear
                |> Time.millisToPosix

        addYear milliseconds =
            milliseconds + 365 * 24 * 60 * 60 * 1000
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
            [ Element.row [ Element.width Element.fill, Element.spacing 20 ] [ summaryInput model ]
            , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ whenInput model ]
            , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ reminderDraft model ]
            , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ createButton model ]
            , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ remindersTable model ]
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


summaryInput : Model -> Element Msg
summaryInput model =
    let
        label =
            Input.labelAbove [ Font.bold, Font.size 14 ] (Element.text "Summary")
    in
    Input.text
        [ borderColor
        , Font.size 20
        , Element.spacing 10
        ]
        { onChange = SetSummary
        , text = model.summary
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


formatTime : Time.Zone -> Time.Posix -> String
formatTime zone time =
    String.concat
        [ String.padLeft 2 '0' <| String.fromInt <| Time.toDay zone time
        , " "
        , Month.toString <| Time.toMonth zone time
        , " "
        , String.fromInt <| Time.toYear zone time
        , ", "
        , String.padLeft 2 '0' <| String.fromInt <| Time.toHour zone time
        , ":"
        , String.padLeft 2 '0' <| String.fromInt <| Time.toMinute zone time
        , " "
        , Weekday.toString <| Time.toWeekday zone time
        ]


reminderDraft : Model -> Element Msg
reminderDraft model =
    case Moment.parse model.when of
        Just moment ->
            Element.text (formatTime model.zone (Moment.toTime model.time model.zone moment))

        Nothing ->
            Element.text ""


createButton : Model -> Element Msg
createButton model =
    let
        optionAttributes : List (Element.Attribute Msg)
        optionAttributes =
            let
                sharedAttrs =
                    [ Border.color (Element.rgb255 217 217 217)
                    , Border.widthEach
                        { bottom = 1
                        , right = 1
                        , left = 1
                        , top = 1
                        }
                    , Element.width Element.fill
                    , Element.padding 13
                    , Font.size 18
                    , Element.pointer
                    , Events.onClick CreateReminder
                    ]
            in
            sharedAttrs
                ++ [ Background.color (Element.rgb255 255 255 255)
                   , Font.color (Element.rgb255 0 0 0)
                   ]

        button =
            Element.el
                optionAttributes
                (Element.el [ Element.centerX, Element.centerY ] (Element.text "Create Reminder"))
    in
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.row [ Element.width Element.fill, Element.spacing 0 ] [ button ]
        ]


remindersTable : Model -> Element Msg
remindersTable model =
    let
        rowPadding =
            Element.padding 10

        rowColor index =
            if modBy 2 index == 0 then
                Background.color (Element.rgb255 255 255 255)

            else
                Background.color (Element.rgb255 245 247 250)

        summaryHeader =
            Element.el
                [ rowColor 1
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
                (Element.text "Summary")

        whenHeader =
            Element.el
                [ rowColor 1
                , Font.bold
                , Border.widthEach
                    { bottom = 1
                    , left = 0
                    , right = 1
                    , top = 0
                    }
                , borderColor
                , rowPadding
                ]
                (Element.text "When")

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
                            [ { header = summaryHeader
                              , width = Element.fill
                              , view = leftColumnView
                              }
                            , { header = whenHeader
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
