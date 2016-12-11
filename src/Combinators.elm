module Combinators exposing
  ( dimap
  , mapHead
  , pure
  , pop
  , push
  )

{-|
Useful combinators for a `ParserPrinter`.

# Maps
@docs dimap, mapHead, pure

# Argument stack
@docs pop, push

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
mapHead : Iso h1 h2 -> P.ParserPrinter r (h1, t) -> P.ParserPrinter r (h2, t)
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
pop : Iso a () -> P.ParserPrinter r (a, t) -> P.ParserPrinter r t
pop iso p =
  let
    dropUnit : Iso ((), t) t
    dropUnit =
      Iso.iso (Tuple.second >> Just) ((,) () >> Just)
  in
    mapHead iso p |> P.map dropUnit


{-| Add an argument to the stack without consuming any input.
-}
push : Iso () a -> P.ParserPrinter r t -> P.ParserPrinter r (a, t)
push iso p =
  let
    addUnit : Iso t ((), t)
    addUnit =
      Iso.iso ((,) () >> Just) (Tuple.second >> Just)
  in
    P.map addUnit p |> mapHead iso

