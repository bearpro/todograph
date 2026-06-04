module Control.TodoGraphItemTest exposing (suite)

import Control.TodoGraphItem exposing (formatSeconds)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "timer duration formatting"
        [ test "formats durations up to 60 minutes as minutes and seconds" <|
            \_ ->
                [ formatSeconds 0
                , formatSeconds 59
                , formatSeconds 60
                , formatSeconds 3600
                ]
                    |> Expect.equal
                        [ "0:00"
                        , "0:59"
                        , "1:00"
                        , "60:00"
                        ]
        , test "adds hours after 60 minutes" <|
            \_ ->
                [ formatSeconds 3601
                , formatSeconds 4515
                ]
                    |> Expect.equal
                        [ "1:00:01"
                        , "1:15:15"
                        ]
        , test "adds days after 23 hours and keeps larger durations in days" <|
            \_ ->
                [ formatSeconds 86400
                , formatSeconds 90915
                , formatSeconds 262515
                ]
                    |> Expect.equal
                        [ "1d 0:00:00"
                        , "1d 1:15:15"
                        , "3d 0:55:15"
                        ]
        ]
