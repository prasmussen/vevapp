module Moment exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import Test exposing (..)
import Time
import Vevapp.Moment as Moment
import Vevapp.Moment.TimeUnit as TimeUnit


suite : Test
suite =
    describe "Moment module"
        [ test "parse clock" <|
            \_ ->
                Expect.equal (Moment.parse "20:05")
                    (Moment.At { hour = 20, minute = 5 })
        , test "tomorrow" <|
            \_ ->
                Expect.equal (Moment.parse "tomorrow")
                    (Moment.Tomorrow Nothing)
        , test "tomorrow and clock" <|
            \_ ->
                Expect.equal (Moment.parse "tomorrow 20:55")
                    (Moment.Tomorrow (Just { hour = 20, minute = 55 }))
        , test "clock and tomorrow" <|
            \_ ->
                Expect.equal (Moment.parse "20:55 tomorrow")
                    (Moment.Tomorrow (Just { hour = 20, minute = 55 }))
        , test "x minutes" <|
            \_ ->
                Expect.equal (Moment.parse "20 minutes")
                    (Moment.In 20 TimeUnit.Minute)
        , test "in x minutes" <|
            \_ ->
                Expect.equal (Moment.parse "in 20 minutes")
                    (Moment.In 20 TimeUnit.Minute)
        , test "in x minute" <|
            \_ ->
                Expect.equal (Moment.parse "in 1 minute")
                    (Moment.In 1 TimeUnit.Minute)
        , test "x minutes from now" <|
            \_ ->
                Expect.equal (Moment.parse "20 minutes from now")
                    (Moment.In 20 TimeUnit.Minute)
        , test "x months" <|
            \_ ->
                Expect.equal (Moment.parse "3 months")
                    (Moment.In 3 TimeUnit.Month)
        , test "xm" <|
            \_ ->
                Expect.equal (Moment.parse "3m")
                    (Moment.In 3 TimeUnit.Minute)
        , test "on day" <|
            \_ ->
                Expect.equal (Moment.parse "on monday")
                    (Moment.On Time.Mon Nothing)
        , test "on day and clock" <|
            \_ ->
                Expect.equal (Moment.parse "on monday 20:32")
                    (Moment.On Time.Mon (Just { hour = 20, minute = 32 }))
        , test "day and clock" <|
            \_ ->
                Expect.equal (Moment.parse "monday 20:32")
                    (Moment.On Time.Mon (Just { hour = 20, minute = 32 }))
        , test "clock and day" <|
            \_ ->
                Expect.equal (Moment.parse "20:32 monday")
                    (Moment.On Time.Mon (Just { hour = 20, minute = 32 }))
        , test "next day" <|
            \_ ->
                Expect.equal (Moment.parse "next monday")
                    (Moment.Next Time.Mon Nothing)
        , test "next day and clock" <|
            \_ ->
                Expect.equal (Moment.parse "next monday 20:32")
                    (Moment.Next Time.Mon (Just { hour = 20, minute = 32 }))
        , test "clock next day" <|
            \_ ->
                Expect.equal (Moment.parse "20:32 next monday")
                    (Moment.Next Time.Mon (Just { hour = 20, minute = 32 }))
        , test "day in month" <|
            \_ ->
                Expect.equal (Moment.parse "1 january")
                    (Moment.Month Time.Jan 1 Nothing)
        , test "day in month abbreviated" <|
            \_ ->
                Expect.equal (Moment.parse "15 sep")
                    (Moment.Month Time.Sep 15 Nothing)
        , test "day in month with clock" <|
            \_ ->
                Expect.equal (Moment.parse "1 january 03:45")
                    (Moment.Month Time.Jan 1 (Just { hour = 3, minute = 45 }))
        , test "month day" <|
            \_ ->
                Expect.equal (Moment.parse "february 28")
                    (Moment.Month Time.Feb 28 Nothing)
        , test "clock month day" <|
            \_ ->
                Expect.equal (Moment.parse "3:45 february 28")
                    (Moment.Month Time.Feb 28 (Just { hour = 3, minute = 45 }))
        , test "day month year" <|
            \_ ->
                Expect.equal (Moment.parse "28 march 2020")
                    (Moment.Date 2020 Time.Mar 28)
        , test "dd.mm.yyyy" <|
            \_ ->
                Expect.equal (Moment.parse "01.03.2020")
                    (Moment.Date 2020 Time.Mar 1)
        , test "d.m.yyyy" <|
            \_ ->
                Expect.equal (Moment.parse "1.3.2020")
                    (Moment.Date 2020 Time.Mar 1)
        , test "d.m.yyyy clock" <|
            \_ ->
                Expect.equal (Moment.parse "1.3.2020 20:00")
                    (Moment.DateTime 2020 Time.Mar 1 { hour = 20, minute = 0 })
        ]
