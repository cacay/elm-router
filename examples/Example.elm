module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import UrlParser exposing ((<$>), (</>), (<?>))
import UrlRouter


main =
    UrlRouter.program app


app : UrlRouter.App Model Route Msg
app =
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    , router = router
    , errorRoute = NotFound
    , newHistoryEntry = \_ _ -> True
    }



-- MODEL


type alias Model =
    ()


init : ( Model, Cmd Msg )
init =
    ( ()
    , Cmd.none
    )



-- ROUTE


type Route
    = Home
    | Page1
    | Page2
    | NotFound


router : UrlParser.Parser a ( Route, a )
router =
    -- We need this to make the app work with elm-reactor
    UrlParser.s "Example.elm"
        </> UrlParser.oneOf
                [ UrlParser.cons0 Home <$> UrlParser.top
                , UrlParser.cons0 Page1 <$> UrlParser.s "page1"
                , UrlParser.cons0 Page2 <$> UrlParser.s "page2"
                ]



-- UPDATE


type alias Msg =
    Never


update : UrlRouter.ChangeRoute Route msg -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    never msg



-- VIEW


view : UrlRouter.Href Route msg -> Model -> Route -> Html msg
view href model route =
    div []
        [ h1 [] [ text "Pages" ]
        , ul [] (List.map (viewLink href) [ Home, Page1, Page2 ])
        , h1 [] [ text "Current Route: ", text <| toString route ]
        ]


viewLink : UrlRouter.Href Route msg -> Route -> Html msg
viewLink href route =
    li [] [ a (href route) [ text <| toString route ] ]
