module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String

import Dict
import UrlSegment


all : Test
all =
    describe "Sample Test Suite"
        [ segment
        ]


segment : Test
segment =
    describe "UrlSegment Parsing and Printing"
        [ describe "Parsing"
            [ test "Top" <|
                \() ->
                    Expect.equal (UrlSegment.fromPath "/") (UrlSegment.empty)
            , test "Empty Top" <|
                \() ->
                    Expect.equal (UrlSegment.fromPath "") (UrlSegment.empty)
            , test "Path Segments" <|
                \() ->
                    Expect.equal (UrlSegment.fromPath "/seg1/seg2") { path = ["seg1", "seg2"], query = Dict.empty }
            , test "Trailing Slash" <|
                \() ->
                    Expect.equal (UrlSegment.fromPath "/seg1/seg2/") { path = ["seg1", "seg2"], query = Dict.empty }

            , test "Only Query String" <|
                \() ->
                    Expect.equal
                      (UrlSegment.fromPath "?key=value")
                      { path = [], query = Dict.fromList [("key", ["value"])] }
            , test "Query String" <|
                \() ->
                    Expect.equal
                      (UrlSegment.fromPath "/seg?key=value")
                      { path = ["seg"], query = Dict.fromList [("key", ["value"])] }
            , test "Preceding Slash" <|
                \() ->
                    Expect.equal
                      (UrlSegment.fromPath "/seg/?key=value")
                      { path = ["seg"], query = Dict.fromList [("key", ["value"])] }
            , test "Multiple Occurrences" <|
                \() ->
                    Expect.equal
                      (UrlSegment.fromPath "/seg/?key=v1&key=v2")
                      { path = ["seg"], query = Dict.fromList [("key", ["v1", "v2"])] }
            , test "Multiple Non-consecutive Occurrences" <|
                \() ->
                    Expect.equal
                      (UrlSegment.fromPath "/seg/?key=v1&name=bob&key=v2")
                      { path = ["seg"], query = Dict.fromList [("key", ["v1", "v2"]), ("name", ["bob"])] }
            ]
        , describe "Printing"
            [ test "Top" <|
                \() ->
                    Expect.equal (UrlSegment.toPath UrlSegment.empty) ""
            , test "Only Path Segments" <|
                \() ->
                    Expect.equal
                      (UrlSegment.toPath { path = ["seg1", "seg2"], query = Dict.empty })
                      "seg1/seg2"

            , test "Only Query String" <|
                \() ->
                    Expect.equal
                      (UrlSegment.toPath { path = [], query = Dict.fromList [("key", ["value"])] })
                      "?key=value"
            , test "Query String" <|
                \() ->
                    Expect.equal
                      (UrlSegment.toPath { path = ["seg"], query = Dict.fromList [("key", ["value"])] })
                      "seg?key=value"
            , test "Multiple Occurrences" <|
                \() ->
                    Expect.equal
                      (UrlSegment.toPath { path = ["seg"], query = Dict.fromList [("key", ["v1", "v2"])] })
                      "seg?key=v1&key=v2"
            ]
        ]

