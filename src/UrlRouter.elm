module UrlRouter exposing
    (Program, application, Application
    , Key, changeRoute, routeUrl
    )

{-| Create an application that manages routing for you.

# Applications

@docs Program, application, Application

# Navigation

@docs changeRoute, routeUrl, Key

-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attribute
import Html.Lazy
import Json.Decode as Json

import Browser exposing (Document)
import Browser.Dom
import Browser.Navigation
import Url exposing (Url)
import Task
import UrlParser
import UrlSegment exposing (Segment)


{-| Stuff you need to supply to `application` to create a `Program`.

The `init`, `view`, `update`, and `subscriptions` fields have the same meaning
as they do in
[`Browser.application`](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application)
--- that is, you should provide what you normally would for those functions.

So, the "special" fields are the `router`, `errorRoute`, and the
`newHistoryEntry` function.

  - `router` will be used to map between the URL and a `route`.

  - `errorRoute` is used when `router` fails to parse a URL entered by the user.
    If the router fails with a route you created, the app will crash as to catch errors
    early. Let me know if you would prefer a different behavior (perhaps send a message to
    your app so you forward that to the server?).

  - `newHistoryEntry` should decide, given the new route and the previous, whether
    a new entry should be created or the latest one modified.

-}
type alias Application flags model route msg =
    { init : flags -> Key -> ( model, Cmd msg )
    , view : model -> route -> Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , router : UrlParser.Parser () ( route, () )
    , errorRoute : route
    , newHistoryEntry : route -> route -> Bool
    }


{-| Like `Platform.Program` but with nicer routing.
-}
type alias Program flags model route msg =
    Platform.Program flags (Model model route) (Msg msg route)


{-| Like `Browser.application` but also handles routing.
-}
application : Application flags model route msg -> Program model route msg
application app =
    Browser.application
        { init = init app
        , update = update app
        , view = view app
        , subscriptions = subscriptions app
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }



-- MODEL


type alias Model userModel route =
    { userModel : userModel
    , route : route
    , rootKey : Browser.Navigation.Key
    , scrolledId : Maybe String
    , retryScroll : Maybe String
    }


init : Application flags model route msg -> flags -> Url -> Key -> ( Model model route, Cmd (Msg msg route) )
init app flags url rootKey =
    let
        key =
            (rootKey, app.router)

        (userModel, userCmd) =
            app.init flags key

        model =
            { userModel = userModel
            , route = app.errorRoute
            , rootKey = rootKey
            , scrolledId = Nothing
            , retryScroll = Nothing
            }

        cmd =
            Cmd.map UserMsg userCmd
    in
    case getRoute app url of
        Nothing ->
            ( model
            , userCmd
            )

        Just (route, normalizedUrl) ->
            if url == normalizedUrl then
                ( { model | route = route }
                , Cmd.batch [ scrollToFragment url, userCmd ]
                )
            else
                ( { model | route = route }
                , Cmd.batch [ Browser.Navigation.replaceUrl (Url.toString normalizedUrl), userCmd ]
                )



-- VIEW


view : Application flags model route msg -> Model model route -> Html (Msg msg route)
view app model =
    Html.Lazy.lazy2 app.view model.userModel model.route
        |> Html.map UserMsg


-- UPDATE


type Msg userMsg route
    = UserMsg userMsg
    | UrlRequest Browser.UrlRequest
    | UrlChange Url
    -- | NewRoute route
    | ScrollSuccess String
    | ScrollFailure String


update : Application flags model route msg -> Msg msg route -> Model model route -> ( Model model route, Cmd (Msg msg route) )
update app msg model =
    case msg of
        UserMsg userMsg ->
            let
                ( newUserModel, userCmd ) =
                    app.update userMsg model.userModel

                retryScroll =
                    case model.retryScroll of
                        Nothing ->
                            Cmd.none

                        Just id ->
                            scrollToId id
            in
            ( if model.userModel == newUserModel then
                model

              else
                { model | userModel = newUserModel }
            , Cmd.batch [ retryScroll, Cmd.map UserMsg userCmd ]
            )

        UrlRequest (Browser.Internal url) ->
            case getRoute url of
                Nothing ->
                    -- This is either a URL we are not meant to handle or
                    -- a bug in the router. We will assume it's not a bug
                    -- and generate a full page load. Hopefully the server
                    -- loads a different app. If not, `init` will supply
                    -- the error route.
                    (model, Browser.Navigation.load <| Url.toString url)

                Just (newRoute, normalizedUrl) ->
                    let
                        -- How do we modify history
                        method =
                            if app.newHistoryEntry model.route newRoute then
                                Browser.Navigation.pushUrl
                            else
                                Browser.Navigation.replaceUrl
                    in
                        (model, method model.rootKey <| Url.toString normalizedUrl)

        UrlRequest (Browser.External url) ->
            (model, Browser.Navigation.load url)

        UrlChange url ->
            case getRoute app url of
                Nothing ->
                    ( { model | route = app.errorRoute }, Cmd.none )

                Just (newRoute, normalizedUrl) ->
                    if url == normalizedUrl then
                        ( { model | route = newRoute, retryScroll = Nothing }
                        , scrollToFragmentIfDifferent model.scrolledId url
                        )
                    else
                        -- TODO: should we update route here? It will be updated
                        -- once the browser URL changes.
                        ( model
                        , Browser.Navigation.replaceUrl model.rootKey normalizedUrl
                        )

        ScrollSuccess id ->
            let
                _ =
                    Debug.log "Scrolled to" id
            in
            ( { model | scrolledId = Just id, retryScroll = Nothing }, Cmd.none )

        ScrollFailure id ->
            let
                _ =
                    Debug.log "Failed to scroll, will retry after a user message" id
            in
            ( { model | retryScroll = Just id }, Cmd.none )


scrollToFragmentIfDifferent : Maybe String -> Url -> Cmd (Msg userMsg route)
scrollToFragmentIfDifferent scrolledId url =
    if scrolledId /= Just url.fragment then
        scrollToFragment url

    else
        Cmd.none


scrollToFragment : Url -> Cmd (Msg userMsg route)
scrollToFragment url =
    scrollToId url.fragment


scrollToId : String -> Cmd (Msg userMsg route)
scrollToId id =
    if not (String.isEmpty id) then
        Browser.Dom.focus id
            |> Task.attempt (\r -> case r of
                Ok () ->
                    ScrollSuccess id
                Err (Browser.Dom.NotFound id) ->
                    ScrollFailure id
            )

    else
        Task.succeed (ScrollSuccess "")
            |> Task.perform identity




-- SUBSCRIPTIONS


subscriptions : Application flags model route msg -> Model model route -> Sub (Msg msg route)
subscriptions app model =
    app.subscriptions model.userModel
        |> Sub.map UserMsg



-- NAVIGATION

{-| Analogous to `Browser.Navigation.Key`. You need to provide this to
`changeRoute` and `routeUrl` functions.
-}
type Key route =
    Key Browser.Navigation.Key (UrlParser.Parser () ( route, () ))



{-| Extract a `Browser.Navigation.Key` from a `Key`. Allows you to call
functions from `Browser.Navigation.
-}
navigationKey : Key route -> Browser.Navigation.Key
navigationKey (Key key _) =
    key


{-| Change current route. This will update the URL accordingly.
-}
changeRoute : Key -> route -> Cmd msg
changeRoute (Key key router) route =
    -- FIXME: this always pushes a new URL.
    Browser.Navigation.pushUrl (routeUrl key route)


{-| Generate an absolute URL from a route. Useful for passing
into `Html.Attributes.href`.
-}
routeUrl : Key -> route -> Cmd msg
routeUrl (Key _ router) route =
    case UrlParser.reverse router route of
        Nothing ->
            crash (reverseError route)

        Just segment ->
            UrlSegment.toAbsolute segment


-- HELPERS


{-| Sometimes you need to change the current route programmatically, without
the user clicking on any links. Your update function can use `ChangeRoute` to
do that. For example, you might extend your `Msg` type with a new case and handle
these in your update function:

    type alias Route = String

    type Msg = ChangeSearchQuery Route | ...

    update : ChangeRoute -> Msg -> Model -> ( Model, Cmd Msg )
    update changeRoute msg model =
        case msg of
            ChangeSearchQuery route ->
                ( model, changeRoute route )

            ...

-}
type alias ChangeRoute route msg =
    route -> Cmd msg



{-| Turn a `Url.Url` into a route. Along with the route, this returns
the canonical URL representation of that route. The browser URL should
be modified to match this.
-}
getRoute : Application flags model route msg -> Url -> Maybe (route, Url)
getRoute app url =
    case UrlParser.parse app.router (UrlSegment.fromUrl url) of
        Nothing ->
            Nothing

        Just route ->
            case UrlParser.reverse app.router route of
                Nothing ->
                    -- This indicates a faulty router.
                    crash <| faultyIsoReverse route url

                Just normalizedSegment ->
                    (route, UrlSegment.updateUrl normalizedSegment url)



-- ERROR MESSAGES


-- Crash the application with the given message.
crash : String -> a
crash =
    Debug.todo


reverseError : route -> String
reverseError route =
    """
Your router failed to reverse a route provided by you.
This could indicate a problem with your router or partial isomorphisms.
The problematic route is: """ ++ Debug.toString route


faultyIsoParse : String -> String
faultyIsoParse url =
    """
Your router failed to parse a route that it created itself (by reversing a route provided to `href`).
This should never happen, and usually indicates a problem with your partial isomorphisms.
The route was generated from: """ ++ url


faultyIsoReverse : route -> Url -> String
faultyIsoReverse route url =
    """
Your router failed to reverse a route that it created itself.
This should never happen, and usually indicates a problem with your partial isomorphisms.
  The route was: """
        ++ Debug.toString route
        ++ "\n"
        ++ "And it was generated from: "
        ++ Url.toString url
