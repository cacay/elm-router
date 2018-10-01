module ParserPrinter.State exposing
    ( State
    , map
    , mapMaybe
    )

import UrlSegment exposing (Segment)


{-| Just wraps a `Segment` and a value; nothing magical.
-}
type alias State a =
    { segment : Segment
    , value : a
    }


map : (a -> b) -> State a -> State b
map f state =
    { state | value = f state.value }


mapMaybe : (a -> Maybe b) -> State a -> Maybe (State b)
mapMaybe f state =
    Maybe.map (\b -> { state | value = b }) (f state.value)
