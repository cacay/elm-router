module ParserPrinter.Parser
    exposing
        ( Parser
        , identity
        , compose
        , empty
        , alternative
        , map
        , path
        , query
        , parse
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
import ParserPrinter.State as State exposing (State)
import UrlSegment exposing (Segment)


-- PARSER


{-| A `Parser a b` takes an `a` to parse a `Segment` and results
in a `b` if parsing succeeds.
-}
type Parser a b
    = Parser (State a -> List (State b))



-- CATEGORY


identity : Parser a a
identity =
    Parser <| \state -> [ state ]


compose : Parser b c -> Parser a b -> Parser a c
compose (Parser p) (Parser q) =
    Parser <| \state -> List.concatMap p (q state)



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
            p state |> List.filterMap (State.mapMaybe <| apply iso)



-- URL


path : Parser a ( String, a )
path =
    Parser <|
        \{ segment, value } ->
            case segment.path of
                [] ->
                    []

                next :: rest ->
                    [ { segment = { segment | path = rest }, value = ( next, value ) } ]


query : String -> Parser a ( List String, a )
query key =
    Parser <|
        \state ->
            let
                values =
                    Maybe.withDefault [] <| Dict.get key state.segment.query
            in
                [ State.map (\a -> ( values, a )) state ]



-- RUNNING


parse : Parser () a -> Segment -> Maybe a
parse (Parser p) seg =
    p { segment = seg, value = () }
        |> List.filter (.segment >> .path >> List.isEmpty)
        |> List.head
        |> Maybe.map .value
