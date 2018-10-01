module ParserPrinter.Parser exposing
    ( Parser, parse
    , identity, compose
    , empty, alternative
    , map
    , path, query
    , composeR, hash
    )

{-|


# Parser

@docs Parser, parse


# Category

@docs identity, compose


# Alternative

@docs empty, alternative


# Iso Functor

@docs map


# Url

@docs path, query

-}

import Dict
import Iso exposing (Iso, apply)
import UrlSegment exposing (Segment)



-- PARSER


{-| A `Parser a b` takes an `a` to parse a `Segment` and results
in a `b` if parsing succeeds.
-}
type Parser a b
    = Parser (Segment -> List ( a -> Maybe b, Segment ))



-- CATEGORY


identity : Parser a a
identity =
    Parser <| \state -> [ ( Basics.identity >> Just, state ) ]


{-| Combine two parsers. The effects happen left to right,
that is, the left parser gets to consume path segments first.
-}
compose : Parser b c -> Parser a b -> Parser a c
compose (Parser p) (Parser q) =
    Parser <|
        \state ->
            List.concatMap
                (\( pf, state2 ) -> List.map (\( qf, state3 ) -> ( qf >> Maybe.andThen pf, state3 )) (q state2))
                (p state)


{-| Combine two parsers in reverse order, but the effects still
happen left to right. So this is significantly different from
`flip compose` when both parsers have effects.
-}
composeR : Parser a b -> Parser b c -> Parser a c
composeR (Parser p) (Parser q) =
    Parser <|
        \state ->
            List.concatMap
                (\( qf, state2 ) -> List.map (\( pf, state3 ) -> ( pf >> Maybe.andThen qf, state3 )) (p state2))
                (q state)



-- ALTERNATIVE


empty : Parser a b
empty =
    Parser <| always []


alternative : Parser a b -> Parser a b -> Parser a b
alternative (Parser p) (Parser q) =
    Parser <| \state -> p state ++ q state



-- ISO FUNCTOR


map : Iso a b -> Parser r a -> Parser r b
map iso (Parser p) =
    Parser <|
        \state ->
            p state |> List.map (\( pf, state2 ) -> ( pf >> Maybe.andThen (apply iso), state2 ))



-- URL


path : Parser a ( String, a )
path =
    Parser <|
        \segment ->
            case segment.path of
                [] ->
                    []

                next :: rest ->
                    [ ( (\b -> ( next, b )) >> Just, { segment | path = rest } ) ]


query : String -> Parser a ( List String, a )
query key =
    Parser <|
        \segment ->
            let
                values =
                    Maybe.withDefault [] <| Dict.get key segment.query
            in
            [ ( (\b -> ( values, b )) >> Just, segment ) ]


hash : Parser a ( Maybe String, a )
hash =
    Parser <|
        \segment ->
            [ ( (\b -> ( segment.hash, b )) >> Just, segment ) ]



-- RUNNING


parse : Parser () a -> Segment -> Maybe a
parse (Parser p) seg =
    p seg
        |> List.filter (Tuple.second >> .path >> List.isEmpty)
        |> List.filterMap (Tuple.first >> (\f -> f ()))
        |> List.head
