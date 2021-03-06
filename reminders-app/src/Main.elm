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


type alias InitializedModel =
    { time : Time.Posix
    , zone : Time.Zone
    , user : Maybe User
    , summary : String
    , when : String
    , reminderTime : Maybe Time.Posix
    , reminders : RemoteData String (List Reminder)
    , error : Maybe Error
    }


type Model
    = InitSuccess InitializedModel
    | InitFailure String


type Msg
    = SetSummary String
    | SetWhen String
    | SignIn
    | SignOut
    | ListReminders
    | CreateReminder
    | FromJavascript Port.MessageFromJavascript
    | FromJavascriptError String


type Error
    = CreateReminderError String


type alias Flags =
    { user : Maybe User
    , error : Maybe String
    }


init : Flags -> ( Task Never Model, Cmd Msg )
init flags =
    let
        task =
            Task.map2 toModel Time.now Time.here

        toModel time zone =
            case flags.error of
                Just error ->
                    InitFailure error

                Nothing ->
                    InitSuccess
                        { time = time
                        , zone = zone
                        , user = flags.user
                        , summary = ""
                        , when = ""
                        , reminderTime = Nothing
                        , reminders = RemoteData.NotAsked
                        , error = Nothing
                        }

        cmd =
            Task.perform (\_ -> ListReminders) (Task.succeed ())
    in
    ( task, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        InitSuccess initializedModel ->
            initializedModel
                |> updateInitialized msg
                |> Tuple.mapFirst InitSuccess

        InitFailure _ ->
            ( model, Cmd.none )


updateInitialized : Msg -> InitializedModel -> ( InitializedModel, Cmd Msg )
updateInitialized msg model =
    case msg of
        SetSummary summary ->
            ( { model | summary = summary }, Cmd.none )

        SetWhen when ->
            let
                reminderTime =
                    when
                        |> Moment.parse
                        |> Maybe.map (Moment.toTime model.time model.zone)
            in
            ( { model | when = when, reminderTime = reminderTime }, Cmd.none )

        ListReminders ->
            listReminders model

        SignIn ->
            ( model, Port.send Port.SignIn )

        SignOut ->
            ( model, Port.send Port.SignOut )

        CreateReminder ->
            case model.reminderTime of
                Nothing ->
                    ( model, Cmd.none )

                Just startTime ->
                    let
                        options =
                            Reminder.create model.summary startTime

                        cmd =
                            Port.send (Port.CreateReminder options)
                    in
                    ( model, cmd )

        FromJavascript jsMsg ->
            updateFromJavascript jsMsg model

        FromJavascriptError err ->
            -- TODO: set error in model
            ( model, Cmd.none )


updateFromJavascript : Port.MessageFromJavascript -> InitializedModel -> ( InitializedModel, Cmd Msg )
updateFromJavascript msg model =
    case msg of
        Port.AuthChange maybeUser ->
            { model | user = maybeUser }
                |> listReminders

        Port.ListRemindersSuccess reminders ->
            ( { model | reminders = RemoteData.Success reminders }, Cmd.none )

        Port.ListRemindersFailure err ->
            ( { model | reminders = RemoteData.Failure err }, Cmd.none )

        Port.CreateReminderSuccess reminder ->
            let
                addReminder reminders =
                    (reminder :: reminders)
                        |> List.sortBy (.startDate >> Time.posixToMillis)

                newReminders =
                    RemoteData.map addReminder model.reminders
            in
            ( { model | reminders = newReminders }, Cmd.none )

        Port.CreateReminderFailure err ->
            ( { model | error = Just (CreateReminderError err) }, Cmd.none )


listReminders : InitializedModel -> ( InitializedModel, Cmd Msg )
listReminders model =
    let
        cmd =
            model.time
                |> listRemindersOptions
                |> Port.ListReminders
                |> Port.send
    in
    case model.user of
        Just _ ->
            ( { model | reminders = RemoteData.Loading }, cmd )

        Nothing ->
            ( model, Cmd.none )


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
    case model of
        InitSuccess initializedModel ->
            viewInitialized initializedModel

        InitFailure error ->
            Html.text ("Failed to initialize google api: " ++ error)


viewInitialized : InitializedModel -> Html Msg
viewInitialized model =
    let
        rows =
            [ Element.column
                [ Element.width Element.fill
                , Element.height (Element.px 42)
                , Border.color (Element.rgb255 217 217 217)
                , Border.widthEach
                    { bottom = 1
                    , right = 0
                    , left = 0
                    , top = 0
                    }
                ]
                [ Element.row
                    [ Element.width (Element.maximum 1000 Element.fill)
                    , Element.centerX
                    , Element.centerY
                    , Element.paddingXY 20 0
                    ]
                    [ topBar model
                    ]
                ]
            , Element.column
                [ Element.width (Element.maximum 1000 Element.fill)
                , Element.centerX
                , Element.spacing 20
                , Element.padding 20
                , Font.size 14
                , Background.color (Element.rgb255 245 247 250)
                ]
                contentRows
            ]

        contentRows =
            case model.user of
                Just _ ->
                    [ Element.row [ Element.width Element.fill, Element.spacing 20 ] [ summaryInput model ]
                    , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ whenInput model ]
                    , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ reminderDraft model ]
                    , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ createButton model ]
                    , Element.row [ Element.width Element.fill, Element.spacing 20 ] [ remindersTable model ]
                    ]

                Nothing ->
                    [ Element.row
                        [ Element.width Element.fill
                        , Element.spacing 20
                        , Element.pointer
                        , Events.onClick SignIn
                        ]
                        [ Element.text "Sign in with google to create reminders" ]
                    ]

        column =
            Element.column
                [ Element.width Element.fill
                ]
                rows
    in
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color (Element.rgb255 245 247 250)
        ]
        column


topBar : InitializedModel -> Element Msg
topBar model =
    let
        authElem =
            case model.user of
                Just user ->
                    Element.el
                        [ Element.alignRight
                        , Element.pointer
                        , Events.onClick SignOut
                        ]
                        (Element.text ("Sign out (" ++ user.email ++ ")"))

                Nothing ->
                    Element.el
                        [ Element.alignRight
                        , Element.pointer
                        , Events.onClick SignIn
                        ]
                        (Element.text "Sign in with google")
    in
    Element.row
        [ Element.width Element.fill
        , Font.size 15
        ]
        [ Element.text "Reminders"
        , authElem
        ]


summaryInput : InitializedModel -> Element Msg
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


whenInput : InitializedModel -> Element Msg
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


reminderDraft : InitializedModel -> Element Msg
reminderDraft model =
    case Moment.parse model.when of
        Just moment ->
            Element.text (formatTime model.zone (Moment.toTime model.time model.zone moment))

        Nothing ->
            Element.text ""


createButton : InitializedModel -> Element Msg
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


remindersTable : InitializedModel -> Element Msg
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
                (Element.link [] { url = reminder.htmlLink, label = Element.text reminder.summary })

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
