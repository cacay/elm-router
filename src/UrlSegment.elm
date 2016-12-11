module UrlSegment exposing
  ( Segment
  , empty
  , merge
  , fromPath
  , toPath
  , fromLocationPath
  , updateLocationPath
  )

{-|

# Segments
@docs Segment, empty, merge

# Parsing and Printing
@docs fromPath, toPath, fromLocationPath, updateLocationPath
-}


import Dict
import Maybe.Extra

import Erl
import Navigation exposing (Location)


{-| A url segment with a path and query parameters.
-}
type alias Segment =
  { path : List String
  , query : Dict.Dict String (List String)
  }


{-| The empty segment.
-}
empty : Segment
empty =
  Segment [] Dict.empty


{-| Merge two segments by appending their paths and taking the union
of their query parameters.
-}
merge: Segment -> Segment -> Segment
merge s1 s2 =
  Segment (s1.path ++ s2.path) (unionWith (++) s1.query s2.query)


unionWith : (a -> a -> a) -> Dict.Dict comparable a -> Dict.Dict comparable a -> Dict.Dict comparable a
unionWith f d1 d2 =
  Dict.merge Dict.insert (\k x1 x2 -> Dict.insert k <| f x1 x2) Dict.insert d1 d2 Dict.empty


insertWith : (a -> a -> a) -> comparable -> a -> Dict.Dict comparable a -> Dict.Dict comparable a
insertWith f k a =
  Dict.update k (\v -> Maybe.Extra.or (Maybe.map (f a) v) (Just a))



-- PARSING and PRINTING

{-| Convert from a `String`. The string should be concatenation of `path`
and `search` components of a `Navigation.Location`.
-}
fromPath : String -> Segment
fromPath url =
  let
    parsed = Erl.parse url
  in
    { path = parsed.path
    , query = List.foldr (\(k, v) -> insertWith (++) k [v]) Dict.empty parsed.query
    }


{-| Convert to a `String`. The string will look like the concatenation of
`path` and `search` components of a `Navigation.Location`.
-}
toPath : Segment -> String
toPath segment =
  let
    splitQuery : List (String, String)
    splitQuery =
      segment.query
        |> Dict.toList
        |> List.concatMap (\(k, values) -> List.map (\v -> (k, v)) values)
  in
    Erl.new
      |> Erl.appendPathSegments segment.path
      |> \erl -> { erl | query = splitQuery }
      |> Erl.toString


{-| Convert from the `path` and `search` components of a `Navigation.Location`.
-}
fromLocationPath : Location -> Segment
fromLocationPath location =
  fromPath (location.pathname ++ location.search)


{-| Replace the `path` and `search` components of a `Navigation.Location`.
-}
updateLocationPath : Segment -> Location -> Location
updateLocationPath segment location =
  let
    printed = toPath segment
  in
    { location
    | pathname = Erl.extractPath printed
    , search = Erl.extractQuery printed
    }

