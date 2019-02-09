port module Vevapp.Port exposing
    ( MessageFromJavascript(..)
    , MessageToJavascript(..)
    , receive
    , send
    )

import Json.Decode as JD
import Json.Decode.Extra as JDE
import Json.Encode as JE
import Vevapp.Reminder as Reminder exposing (Reminder)
import Vevapp.User as User exposing (User)


type MessageToJavascript
    = SignIn
    | SignOut
    | ListReminders Reminder.ListOptions
    | CreateReminder Reminder.CreateOptions


type MessageFromJavascript
    = ListRemindersSuccess (List Reminder)
    | ListRemindersFailure String
    | CreateReminderSuccess Reminder
    | CreateReminderFailure String
    | AuthChange (Maybe User)


type alias JavascriptMsg =
    { tag : String
    , data : JE.Value
    }


jsMsgDecoder : JD.Decoder JavascriptMsg
jsMsgDecoder =
    JD.map2 JavascriptMsg
        (JD.field "tag" JD.string)
        (JD.field "data" JD.value)


send : MessageToJavascript -> Cmd msg
send msg =
    case msg of
        SignIn ->
            toJavascript
                { tag = "SignIn"
                , data = JE.null
                }

        SignOut ->
            toJavascript
                { tag = "SignOut"
                , data = JE.null
                }

        ListReminders options ->
            toJavascript
                { tag = "ListReminders"
                , data = Reminder.encodeListOptions options
                }

        CreateReminder options ->
            toJavascript
                { tag = "CreateReminder"
                , data = Reminder.encodeCreateOptions options
                }



-- TODO: do decoding in main?


receive : (MessageFromJavascript -> msg) -> (String -> msg) -> Sub msg
receive tagger onError =
    let
        decodePayload : JE.Value -> msg
        decodePayload value =
            case JD.decodeValue jsMsgDecoder value of
                Err err ->
                    onError <|
                        String.concat
                            [ "Unexpected message from javascript: "
                            , JD.errorToString err
                            ]

                Ok jsMsg ->
                    case jsMsg.tag of
                        "AuthChange" ->
                            case JD.decodeValue (JDE.maybe User.decoder) jsMsg.data of
                                Ok maybeUser ->
                                    tagger (AuthChange maybeUser)

                                Err err ->
                                    onError (JD.errorToString err)

                        "ListRemindersSuccess" ->
                            case JD.decodeValue Reminder.apiResponseDecoder jsMsg.data of
                                Ok reminders ->
                                    tagger (ListRemindersSuccess reminders)

                                Err err ->
                                    onError (JD.errorToString err)

                        "ListRemindersFailure" ->
                            case JD.decodeValue JD.string jsMsg.data of
                                Ok err ->
                                    tagger (ListRemindersFailure err)

                                Err err ->
                                    onError (JD.errorToString err)

                        "CreateReminderSuccess" ->
                            case JD.decodeValue Reminder.decoder jsMsg.data of
                                Ok reminder ->
                                    tagger (CreateReminderSuccess reminder)

                                Err err ->
                                    onError (JD.errorToString err)

                        "CreateReminderFailure" ->
                            case JD.decodeValue JD.string jsMsg.data of
                                Ok err ->
                                    tagger (CreateReminderFailure err)

                                Err err ->
                                    onError (JD.errorToString err)

                        _ ->
                            onError <|
                                String.concat
                                    [ "Unexpected message from javascript: "
                                    , jsMsg.tag
                                    ]
    in
    fromJavascript decodePayload


port toJavascript : JavascriptMsg -> Cmd msg


port fromJavascript : (JE.Value -> msg) -> Sub msg
