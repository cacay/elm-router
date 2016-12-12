module UrlParser exposing
  ( Parser
  , string, int, s, custom
  , (</>), oneOf, top
  , (<$>), cons0, cons1, cons2, cons3, cons4, cons5
  , (<?>), stringParam, intParam
  , customParam, requiredParam, optionalParam, manyParam
  , parse, reverse
  )

{-|

A reversible URL parser.

# Parser
@docs Parser

# Path Parsers
@docs string, int, s, custom

# Combining Parses
@docs (</>), oneOf, top

# Data Constructors
@docs (<$>), cons0, cons1, cons2, cons3, cons4, cons5

# Query Parameter Parsers
@docs (<?>), stringParam, intParam
@docs customParam, requiredParam, optionalParam, manyParam

# Run a Parser
@docs parse, reverse


# Credits

The interface is heavily influenced by
[evancz/url-parser](http://package.elm-lang.org/packages/evancz/url-parser/2.0.1).
The implementation is based on [Boomerang](https://hackage.haskell.org/package/boomerang-1.4.5.2)
and [this paper](http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf).

-}

import Dict
import Function.Extra
import Maybe.Extra

import Iso exposing (Iso, (<<<), (>>>), (***))
import ParserPrinter as ParserPrinter
import ParserPrinter.Combinators as Combinators
import UrlSegment



-- PARSERS


{-| Turn URLs like `/blog/42/cat-herding-techniques` into nice Elm data and back.
Intuitively, a `Parser a b` takes an `a` to parse a url segment (see `UrlSegment`)
and results in a `b` if parsing succeeds. Additionally, it takes a `b` to print a
segment and returns a `a` if printing succeeds.
-}
type alias Parser a b =
  ParserPrinter.ParserPrinter a b



-- PARSE SEGMENTS


{-| Parse a segment of the path as a `String`.

    parse string location
    -- /alice/  ==>  Just "alice"
    -- /bob     ==>  Just "bob"
    -- /42/     ==>  Just "42"
-}
string : Parser a (String, a)
string =
  custom Iso.string


{-| Parse a segment of the path as an `Int`.

    parse int location
    -- /alice/  ==>  Nothing
    -- /bob     ==>  Nothing
    -- /42/     ==>  Just 42
-}
int : Parser a (Int, a)
int =
  custom Iso.int


{-| Parse a segment of the path if it matches a given string.

    s "blog"  -- can parse /blog/
              -- but not /glob/ or /42/ or anything else
-}
s : String -> Parser a a
s str =
  Combinators.pop (Iso.element str) <$> ParserPrinter.path


{-| Create a custom path segment parser. You need to provide a partial
isomorphism between `String` and your type (see `Iso`). Here is how it
is used to define the `int` and `string` parsers:

    int =
      custom Iso.int

    string =
      custom Iso.string

You can use it to define something like “only CSS files” like this:

    css : Parser String
    css =
      custom <| Iso.subset <| String.endsWith ".css"
-}
custom : Iso String a -> Parser r (a, r)
custom iso =
  Combinators.mapHead iso ParserPrinter.path



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
(</>) : Parser a b -> Parser b c -> Parser a c
(</>) =
  flip ParserPrinter.compose

infixr 7 </>


-- TODO: fix comments.
{-| Try a bunch of different path parsers.

    type Route
      = Search String
      | Blog Int
      | User String
      | Comment String Int

    -- First, define the projection functions. You can think of
    -- these as the opposite of constructors.
    rSearch : Route -> Maybe String
    rSearch route =
      case route of
        Search s ->
          Just s

        _ ->
          Nothing

    -- Similar to `rSearch`.
    rBlog : Route -> Maybe String
    rUser : Route -> Maybe String
    rComment : Route -> Maybe (String, Int)

    route : Parser (Route -> a) a
    route =
      oneOf
        [ cons1 Search rSearch   <$> s "search" </> string
        , cons1 Blog rBlog       <$> s "blog" </> int
        , cons1 User rUser       <$> s "user" </> string
        , cons2 Comment rComment <$> s "user" </> string </> "comments" </> int
        ]

    parse route location
    -- /search/cats           ==>  Just (Search "cats")
    -- /search/               ==>  Nothing

    -- /blog/42               ==>  Just (Blog 42)
    -- /blog/cats             ==>  Nothing

    -- /user/sam/             ==>  Just (User "sam")
    -- /user/bob/comments/42  ==>  Just (Comment "bob" 42)
    -- /user/tom/comments/35  ==>  Just (Comment "tom" 35)
    -- /user/                 ==>  Nothing

-}
oneOf : List (Parser a b) -> Parser a b
oneOf =
  List.foldr ParserPrinter.alternative ParserPrinter.empty


{-| A parser that does not consume any path segments.
You could use it to define optional parsers, for example.

    parse (oneOf [ s "blog", top ] </> int) location
    -- /blog/42  ==>  Just 42
    -- /42       ==>  Just 42
-}
top : Parser a a
top =
  ParserPrinter.identity



-- DATA TYPES

{-| Combine a pure parser with a parser that consumes input.
You will usually use this with data constructors. For example:

    type Route = Search String | User String Int

    -- Projections
    rSearch : Route -> Maybe String
    rUser : Route -> Maybe (String, Int)

    route : Parser a (Route, a)
    route =
      oneOf
        [ cons1 Search rSearch <$> s "search" </> string
        , cons2 User rUser     <$> s "user" </> string </> int
        ]

Under the hood, this is just `(</>)` with the arguments flipped, which
means it can be used with any parser (even impure ones). This looks nicer
though since you can put the constructor first and `(</>)` implies a
forward slash.
-}
(<$>) : Parser b c -> Parser a b -> Parser a c
(<$>) =
  flip (</>)

infixr 7 <$>


{-| A constructor with no arguments.
-}
cons0 : t -> Parser r (t, r)
cons0 t =
  Combinators.push <| Iso.invert <| Iso.element t


{-| A constructor with one argument.
-}
cons1 : (a -> t) -> (t -> Maybe a) -> Parser (a, r) (t, r)
cons1 inj proj =
  Combinators.pure <| Iso.iso (inj >> Just) proj *** Iso.identity


{-| A constructor with two arguments.
-}
cons2 : (a -> b -> t) -> (t -> Maybe (a, b)) -> Parser (a, (b, r)) (t, r)
cons2 inj proj =
  Combinators.pull2 </>
    (Combinators.pure <| Iso.iso (uncurry inj >> Just) proj *** Iso.identity)


{-| A constructor with three arguments.
-}
cons3 : (a -> b -> c -> t) -> (t -> Maybe (a, b, c)) -> Parser (a, (b, (c, r))) (t, r)
cons3 inj proj =
  Combinators.pull3 </>
    (Combinators.pure <| Iso.iso (Function.Extra.uncurry3 inj >> Just) proj *** Iso.identity)


{-| A constructor with four arguments.
-}
cons4 : (a -> b -> c -> d -> t) -> (t -> Maybe (a, b, c, d)) -> Parser (a, (b, (c, (d, r)))) (t, r)
cons4 inj proj =
  Combinators.pull4 </>
    (Combinators.pure <| Iso.iso (Function.Extra.uncurry4 inj >> Just) proj *** Iso.identity)


{-| A constructor with five arguments.
-}
cons5 : (a -> b -> c -> d -> e -> t) -> (t -> Maybe (a, b, c, d, e)) -> Parser (a, (b, (c, (d, (e, r))))) (t, r)
cons5 inj proj =
  Combinators.pull5 </>
    (Combinators.pure <| Iso.iso (Function.Extra.uncurry5 inj >> Just) proj *** Iso.identity)



-- QUERY PARAMETERS


{-| Parse some query parameters. Just a nicer looking alias for `</>`.

    type Route = BlogList (Maybe String) | BlogPost Int

    -- Projections
    rBlogList : Route -> Maybe (Maybe String)
    rBlogPost : Route -> Maybe Int

    route : Parser Route
    route =
      oneOf
        [ cons1 BlogList rBlogList <$> s "blog" <?> stringParam "search"
        , cons1 BlogPost rBlogPost <$> s "blog" </> int
        ]

    parse route location
    -- /blog/              ==>  Just (BlogList Nothing)
    -- /blog/?search=cats  ==>  Just (BlogList (Just "cats"))
    -- /blog/42            ==>  Just (BlogPost 42)
-}
(<?>) : Parser a b -> Parser b c -> Parser a c
(<?>) =
  (</>)

infixr 7 <?>


{-| Parse a query parameter as a `String`.

    parse (s "blog" <?> stringParam "search") location
    -- /blog/              ==>  Just Nothing
    -- /blog/?search=cats  ==>  Just (Just "cats")
-}
stringParam : String -> Parser a (Maybe String, a)
stringParam name =
  optionalParam name Iso.string


{-| Parse a query parameter as an `Int`. Maybe you want to show paginated
search results. You could have a `start` query parameter to say which result
should appear first.

    parse (s "results" <?> intParam "start") location
    -- /results           ==>  Just Nothing
    -- /results?start=10  ==>  Just (Just 10)
-}
intParam : String -> Parser a (Maybe Int, a)
intParam name =
  optionalParam name Iso.int


{-| Create a custom query parser. You get a list af values for
the given key since the same parameter can appear multiple times.
If it isn't in the parameter list, you will get an empty list.
-}
customParam : String -> (Iso (List String) a) -> Parser r (a, r)
customParam name iso =
  Combinators.mapHead iso <| ParserPrinter.query name


{-| Parse a query parameter that is given exactly once. The parser fails
if the parameter is not given or given more than once.
-}
requiredParam : String -> (Iso String a) -> Parser r (a, r)
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

This is usually what you want to use for parameters. In fact, the primitive
parameter parsers are all built with this:

    stringParam =
      optionalParam name Iso.string

    intParam =
      optionalParam name Iso.int
-}
optionalParam : String -> (Iso String a) -> Parser r (Maybe a, r)
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


{-| Parse a query parameter that is given 0 or more times. You need
to provide an isomorphism for the base type. It will be applied to
each occurrence of the query parameter. If any of them fails, the whole
parser fails. Otherwise, you get a list of parsed values. For example:

    parse (s "posts" </> manyParam "user" Iso.int) location
    -- /posts?user=1&user=2  ==>  Just [1, 2]
-}
manyParam : String -> (Iso String a) -> Parser r (List a, r)
manyParam name iso =
  customParam name (Iso.liftList iso)



-- RUN A PARSER


{-| Parse a `UrlSegment.Segment` into an Elm value.
-}
parse : Parser () (a, ()) -> UrlSegment.Segment -> Maybe a
parse p seg =
  ParserPrinter.parse p seg |> Maybe.map Tuple.first


{-| Print a `UrlSegment.Segment` given an Elm value.
-}
reverse : Parser () (a, ()) -> a -> Maybe UrlSegment.Segment
reverse p x =
  ParserPrinter.print p (x, ())

