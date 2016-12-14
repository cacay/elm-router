module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Dict
import Iso
import UrlSegment
import UrlParser exposing (..)
import ParserPrinter exposing (compose)
import ParserPrinter.Combinators exposing (pull2, pull3, pull4)


all : Test
all =
    describe "Sample Test Suite"
        [ segment
        , urlParser
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
                    Expect.equal (UrlSegment.fromPath "/seg1/seg2") { path = [ "seg1", "seg2" ], query = Dict.empty }
            , test "Trailing Slash" <|
                \() ->
                    Expect.equal (UrlSegment.fromPath "/seg1/seg2/") { path = [ "seg1", "seg2" ], query = Dict.empty }
            , test "Only Query String" <|
                \() ->
                    Expect.equal
                        (UrlSegment.fromPath "?key=value")
                        { path = [], query = Dict.fromList [ ( "key", [ "value" ] ) ] }
            , test "Query String" <|
                \() ->
                    Expect.equal
                        (UrlSegment.fromPath "/seg?key=value")
                        { path = [ "seg" ], query = Dict.fromList [ ( "key", [ "value" ] ) ] }
            , test "Preceding Slash" <|
                \() ->
                    Expect.equal
                        (UrlSegment.fromPath "/seg/?key=value")
                        { path = [ "seg" ], query = Dict.fromList [ ( "key", [ "value" ] ) ] }
            , test "Multiple Occurrences" <|
                \() ->
                    Expect.equal
                        (UrlSegment.fromPath "/seg/?key=v1&key=v2")
                        { path = [ "seg" ], query = Dict.fromList [ ( "key", [ "v1", "v2" ] ) ] }
            , test "Multiple Non-consecutive Occurrences" <|
                \() ->
                    Expect.equal
                        (UrlSegment.fromPath "/seg/?key=v1&name=bob&key=v2")
                        { path = [ "seg" ], query = Dict.fromList [ ( "key", [ "v1", "v2" ] ), ( "name", [ "bob" ] ) ] }
            ]
        , describe "Printing"
            [ test "Top" <|
                \() ->
                    Expect.equal (UrlSegment.toPath UrlSegment.empty) "/"
            , test "Only Path Segments" <|
                \() ->
                    Expect.equal
                        (UrlSegment.toPath { path = [ "seg1", "seg2" ], query = Dict.empty })
                        "/seg1/seg2"
            , test "Only Query String" <|
                \() ->
                    Expect.equal
                        (UrlSegment.toPath { path = [], query = Dict.fromList [ ( "key", [ "value" ] ) ] })
                        "/?key=value"
            , test "Query String" <|
                \() ->
                    Expect.equal
                        (UrlSegment.toPath { path = [ "seg" ], query = Dict.fromList [ ( "key", [ "value" ] ) ] })
                        "/seg?key=value"
            , test "Multiple Occurrences" <|
                \() ->
                    Expect.equal
                        (UrlSegment.toPath { path = [ "seg" ], query = Dict.fromList [ ( "key", [ "v1", "v2" ] ) ] })
                        "/seg?key=v1&key=v2"
            ]
        ]


urlParser : Test
urlParser =
    describe "UrlParser"
        [ describe "Parser"
            [ test "Top" <|
                \() ->
                    Expect.equal (parseUrl (cons0 () <$> top) "") (Just ())
            , test "Segment" <|
                \() ->
                    Expect.equal (parseUrl (cons0 () <$> s "seg") "/seg") (Just ())
            , test "Composition" <|
                \() ->
                    Expect.equal (parseUrl (cons0 () <$> s "seg1" </> s "seg2") "/seg1/seg2") (Just ())
            , test "Composition Order" <|
                \() ->
                    Expect.equal (parseUrl (string </> int |> compose pull2) "/seg/42")
                        (Just ( "seg", 42 ))
            , test "Only Query String" <|
                \() ->
                    Expect.equal
                        (parseUrl (intParam "key") "?key=42")
                        (Just (Just 42))
            , test "Query String" <|
                \() ->
                    Expect.equal
                        (parseUrl (s "seg" <?> stringParam "key" <?> intParam "code" |> compose pull2) "/seg?key=value&code=42")
                        (Just ( Just "value", Just 42 ))
            , test "Multiple Occurrences" <|
                \() ->
                    Expect.equal
                        (parseUrl (s "seg" <?> stringParam "key") "/seg/?key=v1&key=v2")
                        Nothing
            ]
        , describe "Printer"
            [ test "Top" <|
                \() ->
                    Expect.equal (printUrl (cons0 () <$> top) ()) (Just "/")
            , test "Segment" <|
                \() ->
                    Expect.equal (printUrl (cons0 () <$> s "seg") ()) (Just "/seg")
            , test "Composition" <|
                \() ->
                    Expect.equal (printUrl (cons0 () <$> s "seg1" </> s "seg2") ()) (Just "/seg1/seg2")
            , test "Composition Order" <|
                \() ->
                    Expect.equal (printUrl (string </> int |> compose pull2) ( "seg", 42 )) (Just "/seg/42")
            , test "Only Query String" <|
                \() ->
                    Expect.equal
                        (printUrl (intParam "key") (Just 42))
                        (Just "/?key=42")
            , test "Query String" <|
                \() ->
                    Expect.equal
                        (printUrl (s "seg" <?> stringParam "key" <?> intParam "code" |> compose pull2) ( Just "value", Just 42 ))
                        (Just "/seg?code=42&key=value")
            ]
        ]



-- HELPERS


parseUrl : UrlParser.Parser () ( a, () ) -> String -> Maybe a
parseUrl p =
    UrlSegment.fromPath >> UrlParser.parse p


printUrl : UrlParser.Parser () ( a, () ) -> a -> Maybe String
printUrl p =
    UrlParser.reverse p >> Maybe.map UrlSegment.toPath
