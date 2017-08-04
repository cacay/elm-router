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


empty : UrlSegment.Segment
empty =
    UrlSegment.empty


segment : Test
segment =
    describe "UrlSegment Parsing and Printing"
        [ describe "Parsing"
            [ test "Top" <|
                \() ->
                    Expect.equal (UrlSegment.fromPath "/") empty
            , test "Empty Top" <|
                \() ->
                    Expect.equal (UrlSegment.fromPath "") empty
            , test "Path Segments" <|
                \() ->
                    Expect.equal (UrlSegment.fromPath "/seg1/seg2") { empty | path = [ "seg1", "seg2" ] }
            , test "Trailing Slash" <|
                \() ->
                    Expect.equal (UrlSegment.fromPath "/seg1/seg2/") { empty | path = [ "seg1", "seg2" ] }
            , test "Only Query String" <|
                \() ->
                    Expect.equal
                        (UrlSegment.fromPath "?key=value")
                        { empty | query = Dict.fromList [ ( "key", [ "value" ] ) ] }
            , test "Query String" <|
                \() ->
                    Expect.equal
                        (UrlSegment.fromPath "/seg?key=value")
                        { empty | path = [ "seg" ], query = Dict.fromList [ ( "key", [ "value" ] ) ] }
            , test "Preceding Slash" <|
                \() ->
                    Expect.equal
                        (UrlSegment.fromPath "/seg/?key=value")
                        { empty | path = [ "seg" ], query = Dict.fromList [ ( "key", [ "value" ] ) ] }
            , test "Multiple Occurrences" <|
                \() ->
                    Expect.equal
                        (UrlSegment.fromPath "/seg/?key=v1&key=v2")
                        { empty | path = [ "seg" ], query = Dict.fromList [ ( "key", [ "v1", "v2" ] ) ] }
            , test "Multiple Non-consecutive Occurrences" <|
                \() ->
                    Expect.equal
                        (UrlSegment.fromPath "/seg/?key=v1&name=bob&key=v2")
                        { empty | path = [ "seg" ], query = Dict.fromList [ ( "key", [ "v1", "v2" ] ), ( "name", [ "bob" ] ) ] }
            , test "Hash" <|
                \() ->
                    Expect.equal
                        (UrlSegment.fromPath "/#hash")
                        { empty | hash = Just "hash" }
            ]
        , describe "Printing"
            [ test "Top" <|
                \() ->
                    Expect.equal (UrlSegment.toPath empty) "/"
            , test "Only Path Segments" <|
                \() ->
                    Expect.equal
                        (UrlSegment.toPath { empty | path = [ "seg1", "seg2" ] })
                        "/seg1/seg2"
            , test "Only Query String" <|
                \() ->
                    Expect.equal
                        (UrlSegment.toPath { empty | query = Dict.fromList [ ( "key", [ "value" ] ) ] })
                        "/?key=value"
            , test "Query String" <|
                \() ->
                    Expect.equal
                        (UrlSegment.toPath { empty | path = [ "seg" ], query = Dict.fromList [ ( "key", [ "value" ] ) ] })
                        "/seg?key=value"
            , test "Multiple Occurrences" <|
                \() ->
                    Expect.equal
                        (UrlSegment.toPath { empty | path = [ "seg" ], query = Dict.fromList [ ( "key", [ "v1", "v2" ] ) ] })
                        "/seg?key=v1&key=v2"
            , test "Hash" <|
                \() ->
                    Expect.equal
                        (UrlSegment.toPath { empty | hash = Just "hash" })
                        "/#hash"
            ]
        ]
