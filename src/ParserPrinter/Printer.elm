module ParserPrinter.Printer exposing
    ( Printer, print
    , identity, compose
    , empty, alternative
    , map
    , path, query
    , composeR, hash
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
import UrlSegment exposing (Segment, merge)



-- PRINTER


{-| A `Printer a b` takes a `b` to print a `Segment` and results
in an `a` if printing succeeds.
-}
type Printer a b
    = Printer (b -> List ( Segment -> Segment, a ))



-- CATEGORY


identity : Printer a a
identity =
    Printer <| \a -> [ ( Basics.identity, a ) ]


{-| Combine two printers. The effects happen left to right,
that is, the output has the first printer's output followed
by the second.
-}
compose : Printer b c -> Printer a b -> Printer a c
compose (Printer p) (Printer q) =
    Printer <|
        \c ->
            List.concatMap (\( pf, b ) -> List.map (\( qf, a ) -> ( qf >> pf, a )) (q b)) (p c)


{-| Combine two printers in reverse order, but the effects still
happen left to right. So this is significantly different from
`flip compose` when both printers have effects.
-}
composeR : Printer a b -> Printer b c -> Printer a c
composeR (Printer p) (Printer q) =
    Printer <|
        \c ->
            List.concatMap (\( qf, b ) -> List.map (\( pf, a ) -> ( qf >> pf, a )) (p b)) (q c)



-- ALTERNATIVE


empty : Printer a b
empty =
    Printer <| always []


alternative : Printer a b -> Printer a b -> Printer a b
alternative (Printer p) (Printer q) =
    Printer <| \b -> p b ++ q b



-- ISO FUNCTOR


map : Iso a b -> Printer r a -> Printer r b
map iso (Printer p) =
    Printer <|
        \b ->
            case unapply iso b of
                Nothing ->
                    []

                Just a ->
                    p a



-- URL


path : Printer a ( String, a )
path =
    Printer <|
        \( pathSegment, a ) ->
            [ ( \segment -> { segment | path = pathSegment :: segment.path }, a ) ]


query : String -> Printer a ( List String, a )
query key =
    Printer <|
        \( params, a ) ->
            -- Note: we do a `Dict.insert` instead of a `UrlSegment.merge` (which does a deep union)
            -- since that would lead to duplicates.
            [ ( \segment -> { segment | query = Dict.insert key params segment.query }, a ) ]


hash : Printer a ( Maybe String, a )
hash =
    Printer <|
        \( h, a ) ->
            [ ( \segment -> { segment | hash = h }, a ) ]



-- RUNNING


print : Printer () a -> a -> Maybe Segment
print (Printer p) a =
    p a
        |> List.head
        |> Maybe.map (\( f, () ) -> f UrlSegment.empty)
