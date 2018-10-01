module UrlSegment exposing
    ( Segment, empty, merge
    , fromAbsolute, toAbsolute, fromUrl, updateUrl
    )

{-|


# Segments

@docs Segment, empty, merge


# Parsing and Printing

@docs fromAbsolute, toAbsolute, fromUrl, updateUrl

-}

import Dict
import Maybe.Extra
import Url exposing (Url)
import Url.Builder


{-| A url segment with a path and query parameters.
-}
type alias Segment =
    { path : List String
    , query : Dict.Dict String (List String)
    , hash : Maybe String
    }


{-| The empty segment.
-}
empty : Segment
empty =
    { path = []
    , query = Dict.empty
    , hash = Nothing
    }


{-| Merge two segments by appending their paths and taking the union
of their query parameters.
-}
merge : Segment -> Segment -> Segment
merge s1 s2 =
    { path = s1.path ++ s2.path
    , query = unionWith (++) s1.query s2.query
    , hash = Maybe.Extra.or s1.hash s2.hash
    }


unionWith : (a -> a -> a) -> Dict.Dict comparable a -> Dict.Dict comparable a -> Dict.Dict comparable a
unionWith f d1 d2 =
    Dict.merge Dict.insert (\k x1 x2 -> Dict.insert k <| f x1 x2) Dict.insert d1 d2 Dict.empty


insertWith : (a -> a -> a) -> comparable -> a -> Dict.Dict comparable a -> Dict.Dict comparable a
insertWith f k a =
    Dict.update k (\v -> Maybe.Extra.or (Maybe.map (f a) v) (Just a))



-- PARSING and PRINTING


{-| Convert an absolute URL (that is composed of path, query, and fragment components)
into a `Segment`. Opposite of `toAbsolute`.
-}
fromAbsolute : String -> Segment
fromAbsolute absolute =
    case Url.fromString ("https:/dummy" ++ absolute) of
        Nothing ->
            empty

        Just url ->
            fromUrl url


{-| Convert a `Segment` into an absolute URL. Opposite of `fromAbsolute`.
-}
toAbsolute : Segment -> String
toAbsolute segment =
    Url.Builder.custom
        Url.Builder.Absolute
        segment.path
        (queryParameters segment.query)
        segment.hash


{-| Extract the relevant components from a `Url` into a `Segment`.
-}
fromUrl : Url -> Segment
fromUrl url =
    { path = parsePath url.path
    , query = parseQueryString url.query
    , hash = url.fragment
    }


{-| Replace relevant components in a `Url` with the ones form a `Segment`.
-}
updateUrl : Segment -> Url -> Url
updateUrl segment url =
    { url
        | path = Url.Builder.absolute segment.path []
        , query =
            let
                queryString =
                    Url.Builder.toQuery (queryParameters segment.query)
            in
                if queryString == "" then
                    Nothing
                else
                    Just queryString
        , fragment = segment.hash
    }



-- LOW LEVEL HELPERS
--
-- Most of this stuff is copied from
-- [elm/url](https://github.com/elm/url/blob/1.0.0/src/Url/Parser.elm).
-- TODO: don't copy-paste.


parsePath : String -> List String
parsePath path =
  -- TODO: do we need to percent decode?
  case String.split "/" path of
    "" :: segments ->
      removeFinalEmpty segments

    segments ->
      removeFinalEmpty segments


removeFinalEmpty : List String -> List String
removeFinalEmpty segments =
  case segments of
    [] ->
      []

    "" :: [] ->
      []

    segment :: rest ->
      segment :: removeFinalEmpty rest


{-| Convert a query string (for example, obtained from the `query` component
of `Url.Url`) into key-value pairs. There may be one or more values per key. Keys
without values and badly encoded keys/values are ignored.

This function applies `Url.percentDecode` to keys and values.
-}
parseQueryString : Maybe String -> Dict.Dict String (List String)
parseQueryString maybeQuery =
  case maybeQuery of
    Nothing ->
      Dict.empty

    Just qry ->
      List.foldr addParam Dict.empty (String.split "&" qry)


addParam : String -> Dict.Dict String (List String) -> Dict.Dict String (List String)
addParam segment dict =
  case String.split "=" segment of
    [rawKey, rawValue] ->
      case Url.percentDecode rawKey of
        Nothing ->
          dict

        Just key ->
          case Url.percentDecode rawValue of
            Nothing ->
              dict

            Just value ->
              Dict.update key (addToParametersHelp value) dict

    _ ->
      dict


addToParametersHelp : a -> Maybe (List a) -> Maybe (List a)
addToParametersHelp value maybeList =
  case maybeList of
    Nothing ->
      Just [value]

    Just list ->
      Just (value :: list)


{-| Generate a list of `Url.Builder.QueryParameter` form a dictionary of parameters.
-}
queryParameters : Dict.Dict String (List String) -> List Url.Builder.QueryParameter
queryParameters query =
    query
        |> Dict.toList
        |> List.concatMap (\( k, values ) -> List.map (\v -> ( k, v )) values)
        |> List.map (\(k, v) ->  Url.Builder.string k v)


