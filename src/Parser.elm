module Parser exposing
  ( Parser
  , parse
  , map
  , product
  , alternative
  , empty
  , pure
  , path
  , query
  )
{-|

# Primitives
@docs Parser, parse, pure, path, query

# IsoFunctor
@docs map

# ProductFunctor
@docs product

# Alternative
@docs alternative, empty

-}


import Dict

import Iso exposing (Iso, apply, unapply)
import UrlSegment exposing (Segment, merge)


-- PARSER

type Parser a =
  Parser (Segment -> List (a, Segment))



-- ISO FUNCTOR

map : Iso a b -> Parser a -> Parser b
map iso (Parser p) =
  Parser <| \seg ->
    List.filterMap (\(a, seg_) -> Maybe.map (\b -> (b, seg_)) <| apply iso a) (p seg)



-- PRODUCT FUNCTOR

product : Parser a -> Parser b -> Parser (a, b)
product (Parser p) (Parser q) =
  Parser <| \seg ->
    List.concatMap (\(x, seg2) -> List.map (Tuple.mapFirst <| (,) x) <| q seg2) (p seg)



-- ALTERNATIVE

alternative : Parser a -> Parser a -> Parser a
alternative (Parser p) (Parser q) =
  Parser <| \seg -> p seg ++ q seg


empty : Parser a
empty =
  Parser <| always []



-- SYNTAX

pure : a -> Parser a
pure x =
  Parser <| \seg -> [(x, seg)]


path : Parser String
path =
  Parser <| \seg ->
    case seg.path of
      [] ->
        []

      next :: rest ->
        [(next, { seg | path = rest })]


query : String -> Parser (List String)
query key =
  Parser <| \seg ->
    [( Maybe.withDefault [] <| Dict.get key seg.query, seg )]



-- RUN A PARSER

parse : Parser a -> Segment -> Maybe a
parse (Parser p) seg =
  p seg
    |> List.filter (Tuple.second >> .path >> List.isEmpty)
    |> List.head
    |> Maybe.map Tuple.first

