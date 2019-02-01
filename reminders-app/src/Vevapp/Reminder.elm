module Vevapp.Reminder exposing
    ( CreateOptions
    , ListOptions
    , Reminder
    , apiResponseDecoder
    , create
    , decoder
    , encodeCreateOptions
    , encodeListOptions
    )

import Iso8601
import Json.Decode as JD
import Json.Encode as JE
import Time


type alias Reminder =
    { summary : String
    , htmlLink : String
    , startDate : Time.Posix
    }


type alias CreateOptions =
    { calendarId : String
    , resource :
        { summary : String
        , start :
            { dateTime : Time.Posix }
        , end :
            { dateTime : Time.Posix }
        , reminders :
            { useDefault : Bool
            , overrides :
                List
                    { method : String
                    , minutes : Int
                    }
            }
        , extendedProperties :
            { private :
                { isReminder : Bool
                }
            }
        }
    }


encodeCreateOptions : CreateOptions -> JE.Value
encodeCreateOptions options =
    let
        encodeOverride { method, minutes } =
            JE.object
                [ ( "method", JE.string method )
                , ( "minutes", JE.int minutes )
                ]
    in
    JE.object
        [ ( "calendarId", JE.string options.calendarId )
        , ( "resource"
          , JE.object
                [ ( "summary", JE.string options.resource.summary )
                , ( "start"
                  , JE.object
                        [ ( "dateTime", JE.string (Iso8601.fromTime options.resource.start.dateTime) ) ]
                  )
                , ( "end"
                  , JE.object
                        [ ( "dateTime", JE.string (Iso8601.fromTime options.resource.end.dateTime) ) ]
                  )
                , ( "reminders"
                  , JE.object
                        [ ( "useDefault", JE.bool options.resource.reminders.useDefault )
                        , ( "overrides", JE.list encodeOverride options.resource.reminders.overrides )
                        ]
                  )
                , ( "extendedProperties"
                  , JE.object
                        [ ( "private"
                          , JE.object
                                [ ( "isReminder", JE.bool options.resource.extendedProperties.private.isReminder ) ]
                          )
                        ]
                  )
                ]
          )
        ]


create : String -> Time.Posix -> CreateOptions
create summary startTime =
    { calendarId = "primary"
    , resource =
        { summary = summary
        , start =
            { dateTime = startTime }
        , end =
            { dateTime = startTime }
        , reminders =
            { useDefault = False
            , overrides =
                [ { method = "email"
                  , minutes = 0
                  }
                , { method = "popup"
                  , minutes = 0
                  }
                ]
            }
        , extendedProperties =
            { private =
                { isReminder = True
                }
            }
        }
    }


apiResponseDecoder : JD.Decoder (List Reminder)
apiResponseDecoder =
    JD.at [ "items" ] (JD.list decoder)


decoder : JD.Decoder Reminder
decoder =
    JD.map3 Reminder
        (JD.field "summary" JD.string)
        (JD.field "htmlLink" JD.string)
        (JD.at [ "start", "dateTime" ] Iso8601.decoder)


type alias ListOptions =
    { calendarId : String
    , maxResults : Int
    , timeMin : String
    , timeMax : String
    , orderBy : String
    , singleEvents : Bool
    , privateExtendedProperty : String
    }


encodeListOptions : ListOptions -> JE.Value
encodeListOptions options =
    JE.object
        [ ( "calendarId", JE.string options.calendarId )
        , ( "maxResults", JE.int options.maxResults )
        , ( "timeMin", JE.string options.timeMin )
        , ( "timeMax", JE.string options.timeMax )
        , ( "orderBy", JE.string options.orderBy )
        , ( "singleEvents", JE.bool options.singleEvents )
        , ( "privateExtendedProperty", JE.string options.privateExtendedProperty )
        ]
