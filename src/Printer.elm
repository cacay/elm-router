module Printer exposing
  ( Printer
  , print
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
@docs Printer, print, pure, path, query

# IsoFunctor
@docs map

# ProductFunctor
@docs product

# Alternative
@docs alternative, empty

-}


import Dict
import Maybe.Extra

import Iso exposing (Iso, apply, unapply)
import UrlSegment exposing (Segment, merge)


-- PRINTER

type Printer a =
  Printer (a -> Maybe Segment)


-- ISO FUNCTOR

map : Iso a b -> Printer a -> Printer b
map iso (Printer p) =
  Printer <| \b -> unapply iso b |> Maybe.andThen p


-- PRODUCT FUNCTOR

product : Printer a -> Printer b -> Printer (a, b)
product (Printer p) (Printer q) =
  Printer <| \(x, y) -> Maybe.map2 merge (p x) (q y)



-- ALTERNATIVE

alternative : Printer a -> Printer a -> Printer a
alternative (Printer p) (Printer q) =
  Printer <| \a -> Maybe.Extra.or (p a) (q a)


empty : Printer a
empty =
  Printer <| always Nothing



-- SYNTAX

pure : a -> Printer a
pure x =
  Printer <| \y -> if x == y then Just UrlSegment.empty else Nothing


path : Printer String
path =
  Printer <| \s -> Just { path = [s], query = Dict.empty }


query : String -> Printer (List String)
query key =
  Printer <| \values -> Just { path = [], query = Dict.singleton key values }



-- RUN A PRINTER

print : Printer a -> a -> Maybe Segment
print (Printer p) x =
  p x

