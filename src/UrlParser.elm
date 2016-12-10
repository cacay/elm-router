module UrlParser exposing
  ( Parser, string, int, s
  , (</>), (<$>), oneOf, top, custom
  , QueryParser, (<?>), stringParam, intParam
  , customParam, requiredParam, optionalParam, manyParam
  , parse, reverse
  )

{-|

# Primitives
@docs Parser, string, int, s

# Path Parses
@docs (</>), (<$>), oneOf, top, custom

# Query Parameter Parsers
@docs QueryParser, (<?>), stringParam, intParam
@docs customParam, requiredParam, optionalParam, manyParam

# Run a Parser
@docs parse, reverse

-}

import Dict
import Maybe.Extra

import Iso exposing (Iso, (<<<), (>>>))
import ParserPrinter
import UrlSegment



-- PARSERS


{-| Turn URLs like `/blog/42/cat-herding-techniques` into nice Elm data.
-}
type alias Parser a =
  ParserPrinter.ParserPrinter a



-- PARSE SEGMENTS


{-| Parse a segment of the path as a `String`.

    parse string location
    -- /alice/  ==>  Just "alice"
    -- /bob     ==>  Just "bob"
    -- /42/     ==>  Just "42"
-}
string : Parser String
string =
  custom Iso.string


{-| Parse a segment of the path as an `Int`.

    parse int location
    -- /alice/  ==>  Nothing
    -- /bob     ==>  Nothing
    -- /42/     ==>  Just 42
-}
int : Parser Int
int =
  custom Iso.int


{-| Parse a segment of the path if it matches a given string.

    s "blog"  -- can parse /blog/
              -- but not /glob/ or /42/ or anything else
-}
s : String -> Parser ()
s =
  custom << Iso.element


{-| Create a custom path segment parser. Here is how it is used to define the
`int` and `string` parsers:

    int =
      custom Iso.int

    string =
      custom Iso.string

You can use it to define something like “only CSS files” like this:

    css : Parser String
    css =
      custom <| Iso.restrict <| String.endsWith ".css"
-}
custom : Iso String a -> Parser a
custom iso =
  ParserPrinter.map iso ParserPrinter.path



-- COMBINING PARSERS


{-| Parse a path with multiple segments.

    parse (s "blog" </> int) location
    -- /blog/35/  ==>  Just 35
    -- /blog/42   ==>  Just 42
    -- /blog/     ==>  Nothing
    -- /42/       ==>  Nothing

    parse (s "search" </> string) location
    -- /search/cats/  ==>  Just "cats"
    -- /search/frog   ==>  Just "frog"
    -- /search/       ==>  Nothing
    -- /cats/         ==>  Nothing
-}
(</>) : Parser a -> Parser b -> Parser (a, b)
(</>) =
  ParserPrinter.product


infixr 7 </>
infixr 6 <$>


{-| Transform a path parser.

    type alias Comment = { author : String, id : Int }

    rawComment : Parser (String -> Int -> a) a
    rawComment =
      s "user" </> string </> s "comments" </> int

    comment : Parser (Comment -> a) a
    comment =
      map Comment rawComment

    parsePath comment location
    -- /user/bob/comments/42  ==>  Just { author = "bob", id = 42 }
    -- /user/tom/comments/35  ==>  Just { author = "tom", id = 35 }
    -- /user/sam/             ==>  Nothing
-}
(<$>) : Iso a b -> Parser a -> Parser b
(<$>) =
  ParserPrinter.map


-- TODO: shortcuts for constructors of different arity.


-- TODO: fix comments.
{-| Try a bunch of different path parsers.

    type Route
      = Search String
      | Blog Int
      | User String
      | Comment String Int

    route : Parser (Route -> a) a
    route =
      oneOf
        [ map Search  (s "search" </> string)
        , map Blog    (s "blog" </> int)
        , map User    (s "user" </> string)
        , map Comment (s "user" </> string </> "comments" </> int)
        ]

    parsePath route location
    -- /search/cats           ==>  Just (Search "cats")
    -- /search/               ==>  Nothing

    -- /blog/42               ==>  Just (Blog 42)
    -- /blog/cats             ==>  Nothing

    -- /user/sam/             ==>  Just (User "sam")
    -- /user/bob/comments/42  ==>  Just (Comment "bob" 42)
    -- /user/tom/comments/35  ==>  Just (Comment "tom" 35)
    -- /user/                 ==>  Nothing

-}
oneOf : List (Parser a) -> Parser a
oneOf =
  List.foldl ParserPrinter.alternative ParserPrinter.empty


{-| A parser that does not consume any path segments.

    type BlogRoute = Overview | Post Int

    blogRoute : Parser (BlogRoute -> a) a
    blogRoute =
      oneOf
        [ map Overview top
        , map Post  (s "post" </> int)
        ]

    parsePath (s "blog" </> blogRoute) location
    -- /blog/         ==>  Just Overview
    -- /blog/post/42  ==>  Just (Post 42)
-}
top : Parser ()
top =
  ParserPrinter.pure ()



-- QUERY PARAMETERS


{-| Turn query parameters like `?name=tom&age=42` into nice Elm data. Just an alias for
`Parser`.
-}
-- TODO: should we bother with an alias? Could be important for precedence or readability.
-- Should we turn it into concrete data instead of an alias?
type alias QueryParser a =
  Parser a


-- TODO: fix example
{-| Parse some query parameters. Just a nicer looking alias for `</>`.

    type Route = BlogList (Maybe String) | BlogPost Int

    route : Parser Route
    route =
      oneOf
        [ map BlogList (s "blog" <?> stringParam "search")
        , map BlogPost (s "blog" </> int)
        ]

    parse route location
    -- /blog/              ==>  Just (BlogList Nothing)
    -- /blog/?search=cats  ==>  Just (BlogList (Just "cats"))
    -- /blog/42            ==>  Just (BlogPost 42)
-}
(<?>) : Parser a -> QueryParser b -> Parser (a, b)
(<?>) =
  (</>)

infixr 8 <?>


{-| Parse a query parameter as a `String`.

    parse (s "blog" <?> stringParam "search") location
    -- /blog/              ==>  Just Nothing
    -- /blog/?search=cats  ==>  Just (Just "cats")
-}
stringParam : String -> QueryParser (Maybe String)
stringParam name =
  optionalParam name Iso.string


{-| Parse a query parameter as an `Int`. Maybe you want to show paginated
search results. You could have a `start` query parameter to say which result
should appear first.

    parse (s "results" <?> intParam "start") location
    -- /results           ==>  Just Nothing
    -- /results?start=10  ==>  Just (Just 10)
-}
intParam : String -> QueryParser (Maybe Int)
intParam name =
  optionalParam name Iso.int


{-| Create a custom query parser. You get a list af values for
the given key since the same parameter can appear multiple times.
If it isn't in the parameter list, you will get an empty list.
-}
customParam : String -> (Iso (List String) a) -> QueryParser a
customParam name iso =
  ParserPrinter.map iso <| ParserPrinter.query name


{-| Parse a query parameter that is given exactly once. The parser fails
if the parameter is not given or given more than once.
-}
requiredParam : String -> (Iso String a) -> QueryParser a
requiredParam name iso =
  let
    matchSingleton : List c -> Maybe c
    matchSingleton l =
      case l of
        [x] ->
          Just x

        _ ->
          Nothing

    one : Iso (List String) String
    one =
      Iso.iso matchSingleton (\x -> Just [x])
  in
    customParam name (iso <<< one)


{-| Parse a query parameter that is given at most once. If the parameter is
missing, you get `Nothing`. If it occurs more than once, the parser fails.

This is usually what you want to use for parameters.
-}
optionalParam : String -> (Iso String a) -> QueryParser (Maybe a)
optionalParam name iso =
  let
    matchSingleton : List c -> Maybe (Maybe c)
    matchSingleton l =
      case l of
        [] ->
          Just Nothing

        [x] ->
          Just <| Just x

        _ ->
          Nothing

    one : Iso (List String) (Maybe String)
    one =
      Iso.iso matchSingleton (Maybe.Extra.maybeToList >> Just)
  in
    customParam name (Iso.liftMaybe iso <<< one)


{-| Parse a query parameter that is given 0 or more times.
-}
manyParam : String -> (Iso String a) -> QueryParser (List a)
manyParam name iso =
  customParam name (Iso.liftList iso)



-- RUN A PARSER


{-| Parse a `UrlSegment.Segment`.
-}
parse : Parser a -> UrlSegment.Segment -> Maybe a
parse =
  ParserPrinter.parse


{-| Print a `UrlSegment.Segment`.
-}
reverse : Parser a -> a -> Maybe UrlSegment.Segment
reverse =
  ParserPrinter.print

