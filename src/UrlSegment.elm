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
import Http
import Maybe.Extra
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
  case String.split "?" url of
    [path, query] ->
      Segment (parsePath path) (parseQuery query)

    _ ->
      Segment (parsePath url) Dict.empty


{-| Convert to a `String`. The string will look like the concatenation of
`path` and `search` components of a `Navigation.Location`.
-}
toPath : Segment -> String
toPath segment =
  let
    path = String.cons '/' <| printPath segment.path
    query = printQuery segment.query
  in
    if query == "" then
      path
    else
      path ++ "?" ++ query


{-| Convert from the `path` and `search` components of a `Navigation.Location`.
-}
fromLocationPath : Location -> Segment
fromLocationPath location =
  Segment (parsePath location.pathname) (location.search |> String.dropLeft 1 |> parseQuery)


{-| Replace the `path` and `search` components of a `Navigation.Location`.
-}
updateLocationPath : Segment -> Location -> Location
updateLocationPath segment location =
  { location
  | pathname = String.cons '/' <| printPath segment.path
  , search = String.cons '?' <| printQuery segment.query
  }



-- PARSER HELPERS


parsePath : String -> List String
parsePath url =
 List.filter (not << String.isEmpty) <| String.split "/" url


parseQuery : String -> Dict.Dict String (List String)
parseQuery queryString =
  queryString
    |> String.split "&"
    |> List.filterMap toKeyValuePair
    |> List.foldr (\(k, v) -> insertWith (++) k [v]) Dict.empty


toKeyValuePair : String -> Maybe (String, String)
toKeyValuePair segment =
  case String.split "=" segment of
    [key, value] ->
      Maybe.map2 (,) (Http.decodeUri key) (Http.decodeUri value)

    _ ->
      Nothing


printPath : List String -> String
printPath path =
  String.join "/" path


printQuery : Dict.Dict String (List String) -> String
printQuery query =
  Dict.toList query
    |> List.concatMap (\(k, values) -> List.map (\v -> (k, v)) values)
    |> List.map (\(k, v) -> Http.encodeUri k ++ "=" ++ Http.encodeUri v)
    |> String.join "&"

