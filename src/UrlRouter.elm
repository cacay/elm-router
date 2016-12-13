module UrlRouter exposing
  ( App
  , Program
  , program
  , href
  )

{-| Create a `Program` that manages routing for you.

@docs Program, program, App, href

-}

import Html exposing (Html, Attribute)
import Html.Attributes as Attribute
import Html.Events as Events
import Json.Decode as Json
import Navigation exposing (Location)

import UrlParser
import UrlSegment exposing (Segment)
import LinkMonitor


{-| The configuration required to use this module to create a `Program`.

The `init`, `update`, `subscriptions` and `view` fields have the same meaning
as they do in [`Html.program`](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#program)
--- that is, you should provide what you normally provide to that function.

So, the "special" fields are the `router`, `errorRoute`, and the
`newHistoryEntry` function.

* `router` will be used to map between the URL and a `route`.

* `errorRoute` is used when `router` fails to parse a URL entered by the user.
  If the router fails with a route you created, the app will crash as to catch errors
  early. Let me know if you would prefer a different behavior (maybe send a message to
  your app so you forward that to the servers?).

* `newHistoryEntry` should decide, given the new route and the previous, whether
   a new entry should be created or the latest one modified.
-}
type alias App model route msg =
  { init : ( model, Cmd msg )
  , update : msg -> model -> ( model, Cmd msg )
  , subscriptions : model -> Sub msg
  , view : model -> route -> Html msg
  , router : UrlParser.Parser () (route, ())
  , errorRoute : route
  , newHistoryEntry : route -> route -> Bool
  }


{-| Like `Platform.Program` but also handles routing.
-}
type alias Program model route msg =
  Platform.Program Never (Model model route) (Msg msg route)


{-| Like `Platform.program` but also handles routing.
-}
program : App model route msg -> Program model route msg
program app =
  Navigation.program
    NewUrl
    { init = init app
    , update = update app
    , view = view app
    , subscriptions = subscriptions app
    }



-- MODEL

type alias Model userModel route =
  { userModel : userModel
  , route : route
  , reportedUrl : Location
  }


init : App model route msg -> Location -> ( Model model route, Cmd (Msg msg route) )
init app location =
  let
    userCmd = Cmd.map UserMsg (Tuple.second app.init)
  in
    case getRoute app location of
      Just (route, navigate) ->
        ( { userModel = Tuple.first app.init, route = route, reportedUrl = location }
        , Cmd.batch [ navigate, userCmd ]
        )

      Nothing ->
        ( { userModel = Tuple.first app.init, route = app.errorRoute, reportedUrl = location }
        , userCmd
        )



-- UPDATE

type Msg userMsg route
  = UserMsg userMsg
  | NewUrl Location
  | NewRoute route
  | LinkClick String


update : App model route msg -> Msg msg route -> Model model route -> ( Model model route, Cmd (Msg msg route) )
update app msg model =
  case msg of
    UserMsg userMsg ->
      let
        ( newUserModel, userCmd ) = app.update userMsg model.userModel
      in
        ( { model | userModel = newUserModel }, Cmd.map UserMsg userCmd )

    NewUrl location ->
      case getRoute app location of
        Nothing ->
          ( { model | route = app.errorRoute, reportedUrl = location }, Cmd.none )

        Just (route, cmd) ->
          ( {model | route = route }, cmd )

    NewRoute newRoute ->
      let
        url : String
        url =
          case UrlParser.reverse app.router newRoute of
            Nothing ->
              Debug.crash <| reverseError newRoute

            Just segment ->
              UrlSegment.toPath segment

        changeUrl : String -> Cmd a
        changeUrl =
          if app.newHistoryEntry model.route newRoute then
            Navigation.newUrl
          else
            Navigation.modifyUrl
      in
        -- TODO: should we update route here? It will also be updated once
        -- we get the NewUrl message that we just set off... Depending on the
        -- order of events, one or the other is more correct.
        ( { model | route = newRoute }, changeUrl url )

    LinkClick url ->
      case UrlParser.parse app.router (UrlSegment.fromPath url) of
        Nothing ->
          -- TODO: crashing here might be a bad idea since users could
          -- manually modify the `href` attribute to an invalid url and
          -- cause the app to crash. But if they are modifying stuff, then
          -- all bets are off... Better to catch potential mistakes in the
          -- parser.
          Debug.crash <| faultyIsoParse url

        Just route ->
          update app (NewRoute route) model



-- VIEW

view : App model route msg -> Model model route -> Html (Msg msg route)
view app model =
  app.view model.userModel model.route
    |> Html.map UserMsg



-- SUBSCRIPTIONS

isNavigationClick : Json.Decoder ()
isNavigationClick =
  let
    expect : a -> Json.Decoder a -> Json.Decoder ()
    expect a decoder =
      decoder
        |> Json.andThen (\b ->
          if a == b then
            Json.succeed ()
          else
            Json.fail <| "expected " ++ toString a ++ " but found " ++ toString b
        )

    all : List (Json.Decoder ()) -> Json.Decoder ()
    all decoders =
      List.foldr (\l acc -> l |> Json.andThen (always acc)) (Json.succeed ()) decoders
  in
    all
      [ expect "true" <| Json.at ["target", "dataset", "elmRouter"] Json.string
      , expect 0 <| Json.field "button" Json.int
        -- TODO: modifier keys
      ]


subscriptions : App model route msg -> Model model route -> Sub (Msg msg route)
subscriptions app model =
  Sub.batch
    [ LinkMonitor.clicks isNavigationClick LinkClick
    , app.subscriptions model.userModel |> Sub.map UserMsg
    ]



-- HELPERS

{-| Attach a link to a node that will look like an ordinary link to the browser.
In general, the browser can display the target (unlike "links" added using `onClick`
handlers), and all usual click actions (right-click, modifier+click etc.) will work
as expected. Only clicks that cause navigation (e.g. left click without modifiers)
will be caught and passed along to your app as a new route so that they don't cause
a page reload.
-}
href : App model route msg -> route -> List (Attribute a)
href app route =
  let
    href : String
    href =
      case UrlParser.reverse app.router route of
        Nothing ->
          Debug.crash <| reverseError route

        Just segment ->
          UrlSegment.toPath segment
  in
    [ Attribute.href href
    , Attribute.attribute "data-elm-router" "true"
    ]


{-| Turn a location into a route. We also return a command in case we need to `normalize`
the browser url.
-}
getRoute : App model route msg -> Location -> Maybe (route, Cmd a)
getRoute app location =
  let
    getPath : Location -> String
    getPath location =
      location.pathname ++ location.search
  in
    case UrlParser.parse app.router (UrlSegment.fromLocationPath location) of
      Nothing ->
        Nothing

      Just route ->
        case UrlParser.reverse app.router route of
          Nothing ->
            Debug.crash <| faultyIsoReverse route (getPath location)

          Just normalizedSegment ->
            if getPath location == UrlSegment.toPath normalizedSegment then
              -- Browser url is already normalized
              Just ( route, Cmd.none )
            else
              -- We need to normalize the browser url
              Just (route, Navigation.modifyUrl <| UrlSegment.toPath normalizedSegment)



-- ERROR MESSAGES

reverseError : route -> String
reverseError route = """
Your router failed to reverse a route provided by you.
This could indicate a problem with your router or partial isomorphisms.
The problematic route is: """ ++ toString route


faultyIsoParse : String -> String
faultyIsoParse url = """
Your router failed to parse a route that it created itself (by reversing a route provided to `href`).
This should never happen, and usually indicates a problem with your partial isomorphims.
The route was generated from: """ ++ url


faultyIsoReverse : route -> String -> String
faultyIsoReverse route url = """
Your router failed to reverse a route that it created itself.
This should never happen, and usually indicates a problem with your partial isomorphims.
  The route was: """ ++ toString route ++ "\n" ++
  "And it was generated from: " ++ url
