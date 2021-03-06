module Iso
    exposing
        ( Iso
        , iso
        , apply
        , unapply
        , invert
        , identity
        , (<<<)
        , (>>>)
        , (***)
        , commute
        , associate
        , liftMaybe
        , liftList
        , ignore
        , element
        , subset
        , string
        , int
        )

{-|
Partial isomorphisms between types. These are like functions,
but they go in both directions, and they might fail to produce a
value.

# Partial Isomorphisms
@docs Iso, iso, apply, unapply, invert

# Category
@docs identity, (<<<), (>>>)

# Lifting to Type Constructors
@docs (***), commute, associate, liftMaybe, liftList

# Combinators
@docs ignore, element, subset

# Base types
@docs string, int

-}

import Maybe.Extra


{-| A partial isomorphism between two types.
-}
type Iso a b
    = Iso (a -> Maybe b) (b -> Maybe a)


{-| Construct an isomorphism given functions in both directions.
-}
iso : (a -> Maybe b) -> (b -> Maybe a) -> Iso a b
iso =
    Iso


{-| Apply an isomorphism forwards.
-}
apply : Iso a b -> a -> Maybe b
apply (Iso f g) =
    f


{-| Apply an isomorphism backwards.
-}
unapply : Iso a b -> b -> Maybe a
unapply (Iso f g) =
    g


{-| Reverse an isomorphism.
-}
invert : Iso a b -> Iso b a
invert (Iso f g) =
    Iso g f


{-| The identity isomorphism.
-}
identity : Iso a a
identity =
    Iso Just Just


{-| Compose two isomorphisms.
-}
(<<<) : Iso b c -> Iso a b -> Iso a c
(<<<) g f =
    Iso (apply f >> Maybe.andThen (apply g)) (unapply g >> Maybe.andThen (unapply f))


{-| Compose two isomorphisms in the other direction.
-}
(>>>) : Iso a b -> Iso b c -> Iso a c
(>>>) =
    flip (<<<)


infixr 9 <<<


infixl 9 >>>



-- LIFTING TO TYPE CONSTRUCTORS


{-| Combine two separate isomorphisms to work on the two components
of a tuple.
-}
(***) : Iso a c -> Iso b d -> Iso ( a, b ) ( c, d )
(***) fst snd =
    Iso
        (\( a, b ) -> Maybe.map2 (,) (apply fst a) (apply snd b))
        (\( c, d ) -> Maybe.map2 (,) (unapply fst c) (unapply snd d))


{-| Products commute.
-}
commute : Iso ( a, b ) ( b, a )
commute =
    Iso (\( a, b ) -> Just ( b, a )) (\( b, a ) -> Just ( a, b ))


{-| Nested Products associate.
-}
associate : Iso ( a, ( b, c ) ) ( ( a, b ), c )
associate =
    Iso (\( a, ( b, c ) ) -> Just ( ( a, b ), c )) (\( ( a, b ), c ) -> Just ( a, ( b, c ) ))


{-| Lift an isomorphism over `Maybe`. The resulting isomorphism identifies `Nothing`
with `Nothing`, and it identifies `Just a` with `Just b` if and only if `a` is
isomorphic to `b` according to the provided isomorphism.
-}
liftMaybe : Iso a b -> Iso (Maybe a) (Maybe b)
liftMaybe iso =
    let
        applyMaybe : (c -> Maybe d) -> Maybe c -> Maybe (Maybe d)
        applyMaybe f m =
            case m of
                Nothing ->
                    Just Nothing

                Just a ->
                    case f a of
                        Nothing ->
                            Nothing

                        Just b ->
                            Just (Just b)
    in
        Iso (applyMaybe <| apply iso) (applyMaybe <| unapply iso)


{-| Lift an isomorphism over `List`. `[a_1, a_2, ..., a_n]` will be isomorphic to
`[b_1, b_2, ..., b_n]` if and only if `a_i` is isomorphic to `b_i` for each i.
-}
liftList : Iso a b -> Iso (List a) (List b)
liftList iso =
    Iso (Maybe.Extra.traverse <| apply iso) (Maybe.Extra.traverse <| unapply iso)



-- COMBINATORS


{-| Isomorphism between a set and `()`. Essentially, treats the
whole set as one equivalence class (considers all elements equal).
-}
ignore : a -> Iso a ()
ignore x =
    Iso (always <| Just ()) (always <| Just x)


{-| `element x` is the partial isomorphism between the singleton set
which contains only `x` and `()`.
-}
element : a -> Iso a ()
element x =
    Iso
        (\y ->
            if x == y then
                Just ()
            else
                Nothing
        )
        (always <| Just x)


{-| For a predicate `p`, `subset p` is the identity isomorphism
restricted to elements matching the predicate.
-}
subset : (a -> Bool) -> Iso a a
subset p =
    let
        check x =
            if p x then
                Just x
            else
                Nothing
    in
        Iso check check



-- BASE TYPES


{-| Isomorphism between `String` and `String`, i.e. identity.
-}
string : Iso String String
string =
    identity


{-| Partial isomorphism between `String` and `Int`
-}
int : Iso String Int
int =
    Iso (String.toInt >> Result.toMaybe) (toString >> Just)
