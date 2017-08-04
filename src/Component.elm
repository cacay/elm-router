module Component
    exposing
        ( Component
        , AppMsg
        )

{-|

# Components
@docs Component, AppMsg

-}

import Html exposing (Html)
import UrlParser
import UrlRouter exposing (ChangeRoute, Href)


{-| A sub-component.
-}
type alias Component state route msg model appMsg =
    { init : ( state, Cmd msg )
    , update : AppMsg msg appMsg -> ChangeRoute route appMsg -> msg -> state -> ( state, Cmd appMsg )
    , subscriptions : state -> Sub msg
    , view : AppMsg msg appMsg -> Href route appMsg -> state -> route -> model -> Html appMsg
    , router : UrlParser.Parser () ( route, () )
    , newHistoryEntry : route -> route -> Bool
    }


{-| Turn a `Component` message into a top level message.
-}
type alias AppMsg msg appMsg =
    msg -> appMsg
