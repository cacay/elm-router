module ParserPrinter.Combinators
    exposing
        ( dimap
        , mapHead
        , pure
        , pop
        , push
        , pull2
        , pull3
        , pull4
        , pull5
        , pull6
        )

{-|
Useful combinators for a `ParserPrinter`.

# Maps
@docs dimap, mapHead, pure

# Argument stack
@docs pop, push
@docs pull2, pull3, pull4, pull5, pull6

-}

import Iso exposing (Iso, (***))
import ParserPrinter as P


-- MAPS


{-| Map both the input and the output.
-}
dimap : Iso c a -> Iso b d -> P.ParserPrinter a b -> P.ParserPrinter c d
dimap iso1 iso2 p =
    pure iso1 |> P.compose (P.map iso2 p)


{-| Map the top argument on the argument stack.
-}
mapHead : Iso h1 h2 -> P.ParserPrinter r ( h1, t ) -> P.ParserPrinter r ( h2, t )
mapHead iso p =
    P.map (iso *** Iso.identity) p


{-| A `ParserPrinter` that does not consume any arguments.
-}
pure : Iso a b -> P.ParserPrinter a b
pure iso =
    P.map iso P.identity



-- MANAGING ARGUMENT STACK


{-| Drop the argument at the top of the stack.
-}
pop : Iso h () -> P.ParserPrinter ( h, t ) t
pop iso =
    let
        dropUnit : Iso ( (), t ) t
        dropUnit =
            Iso.iso (Tuple.second >> Just) ((,) () >> Just)
    in
        pure (iso *** Iso.identity)
            |> P.compose (pure dropUnit)


{-| Add an argument to the stack without consuming any input.
-}
push : Iso () h -> P.ParserPrinter t ( h, t )
push iso =
    let
        addUnit : Iso t ( (), t )
        addUnit =
            Iso.iso ((,) () >> Just) (Tuple.second >> Just)
    in
        pure addUnit
            |> P.compose (pure <| iso *** Iso.identity)


{-| Tuple up the top two arguments.
-}
pull2 : P.ParserPrinter ( a, ( b, r ) ) ( ( a, b ), r )
pull2 =
    pure Iso.associate


{-| Tuple up the top three arguments.
-}
pull3 : P.ParserPrinter ( a, ( b, ( c, r ) ) ) ( ( a, b, c ), r )
pull3 =
    pull2
        |> P.compose
            (pure <|
                Iso.iso
                    (\( ( a, b ), ( c, r ) ) -> Just ( ( a, b, c ), r ))
                    (\( ( a, b, c ), r ) -> Just ( ( a, b ), ( c, r ) ))
            )


{-| Tuple up the top four arguments.
-}
pull4 : P.ParserPrinter ( a, ( b, ( c, ( d, r ) ) ) ) ( ( a, b, c, d ), r )
pull4 =
    pull3
        |> P.compose
            (pure <|
                Iso.iso
                    (\( ( a, b, c ), ( d, r ) ) -> Just ( ( a, b, c, d ), r ))
                    (\( ( a, b, c, d ), r ) -> Just ( ( a, b, c ), ( d, r ) ))
            )


{-| Tuple up the top five arguments.
-}
pull5 : P.ParserPrinter ( a, ( b, ( c, ( d, ( e, r ) ) ) ) ) ( ( a, b, c, d, e ), r )
pull5 =
    pull4
        |> P.compose
            (pure <|
                Iso.iso
                    (\( ( a, b, c, d ), ( e, r ) ) -> Just ( ( a, b, c, d, e ), r ))
                    (\( ( a, b, c, d, e ), r ) -> Just ( ( a, b, c, d ), ( e, r ) ))
            )


{-| Tuple up the top six arguments.
-}
pull6 : P.ParserPrinter ( a, ( b, ( c, ( d, ( e, ( f, r ) ) ) ) ) ) ( ( a, b, c, d, e, f ), r )
pull6 =
    pull5
        |> P.compose
            (pure <|
                Iso.iso
                    (\( ( a, b, c, d, e ), ( f, r ) ) -> Just ( ( a, b, c, d, e, f ), r ))
                    (\( ( a, b, c, d, e, f ), r ) -> Just ( ( a, b, c, d, e ), ( f, r ) ))
            )
