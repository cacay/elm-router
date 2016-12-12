module ParserPrinter.Printer exposing
  ( Printer
  , identity, compose
  , empty, alternative
  , map
  , path, query
  , print
  )

{-|

# Printer
@docs Printer, print

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

import Iso exposing (Iso, unapply)
import ParserPrinter.State as State exposing (State)
import UrlSegment exposing (Segment, merge)


-- PRINTER

{-| A `Printer a b` takes a `b` to print a `Segment` and results
in an `a` if printing succeeds.
-}
type Printer a b =
  Printer (State b -> List (State a))



-- CATEGORY

identity : Printer a a
identity =
  Printer <| \state -> [state]


compose : Printer b c -> Printer a b -> Printer a c
compose (Printer p) (Printer q) =
  Printer <| \state -> List.concatMap q (p state)



-- ALTERNATIVE

empty : Printer a b
empty =
  Printer <| always []


alternative : Printer a b -> Printer a b -> Printer a b
alternative (Printer p) (Printer q) =
  Printer <| \state -> p state ++ q state



-- ISO FUNCTOR

map : Iso a b -> Printer r a -> Printer r b
map iso (Printer p) =
  Printer <| \state ->
    case unapply iso state.value of
      Nothing ->
        []

      Just a ->
        p { state | value = a }



-- URL

path : Printer a (String, a)
path =
  Printer <| \{ segment, value } ->
    let
      newSegment = { segment | path = Tuple.first value :: segment.path }
    in
      [ { segment = newSegment, value = Tuple.second value } ]


query : String -> Printer a (List String, a)
query key =
  Printer <| \{segment, value} ->
    let
      -- Note: we do a `Dict.insert` instead of a `UrlSegment.merge` (which does a deep union)
      -- since that would lead to duplicates.
      newSegment = { segment | query = Dict.insert key (Tuple.first value) segment.query }
    in
      [ { segment = newSegment, value = Tuple.second value } ]



-- RUNNING

print : Printer () a -> a -> Maybe Segment
print (Printer p) a =
  p { segment = UrlSegment.empty, value = a }
    |> List.head
    |> Maybe.map .segment

