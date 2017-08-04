module UrlRouter
    exposing
        ( App
        , Program
        , program
        , Href
        , ChangeRoute
        )

{-| Create a `Program` that manages routing for you.

@docs Program, program, App, Href, ChangeRoute

-}

import Html exposing (Html, Attribute)
import Html.Lazy
import Html.Attributes as Attribute
import Task
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
    , update : ChangeRoute route msg -> msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : Href route msg -> model -> route -> Html msg
    , router : UrlParser.Parser () ( route, () )
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
    , scrolledId : Maybe LinkMonitor.Id
    , retryScroll : Maybe LinkMonitor.Id
    }


init : App model route msg -> Location -> ( Model model route, Cmd (Msg msg route) )
init app location =
    let
        userCmd =
            Cmd.map UserMsg (Tuple.second app.init)

        model =
            { userModel = Tuple.first app.init
            , route = app.errorRoute
            , reportedUrl = location
            , scrolledId = Nothing
            , retryScroll = Nothing
            }
    in
        case getRoute app location of
            NotFound ->
                ( model
                , userCmd
                )

            Found route ->
                ( { model | route = route }
                , Cmd.batch [ scrollToHash location, userCmd ]
                )

            FoundWithRedirect route redirect ->
                ( { model | route = route }
                , Cmd.batch [ redirect, userCmd ]
                )



-- UPDATE


type Msg userMsg route
    = UserMsg userMsg
    | NewUrl Location
    | NewRoute route
    | LinkClick String
    | ScrollSuccess LinkMonitor.Id
    | ScrollFailure LinkMonitor.Id


update : App model route msg -> Msg msg route -> Model model route -> ( Model model route, Cmd (Msg msg route) )
update app msg model =
    case msg of
        UserMsg userMsg ->
            let
                ( newUserModel, userCmd ) =
                    app.update (changeRoute app model.route) userMsg model.userModel

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

        NewUrl location ->
            case getRoute app location of
                NotFound ->
                    ( { model | route = app.errorRoute, reportedUrl = location }, Cmd.none )

                Found newRoute ->
                    ( { model | route = newRoute, reportedUrl = location, retryScroll = Nothing }
                    , scrollToHashIfDifferent model.scrolledId location
                    )

                FoundWithRedirect newRoute redirect ->
                    ( { model | route = newRoute, reportedUrl = location }
                    , redirect
                    )

        NewRoute newRoute ->
            -- TODO: should we update route here? It will also be updated once
            -- we get the NewUrl message that we just set off... Depending on the
            -- order of events, one or the other could be more correct. But this way,
            -- we only have one point of entry for route changes.
            ( model, changeRoute app model.route newRoute )

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

        ScrollSuccess id ->
            let
                _ =
                    if False then
                        Debug.log "Scrolled to" id
                    else
                        ""
            in
                ( { model | scrolledId = Just id, retryScroll = Nothing }, Cmd.none )

        ScrollFailure id ->
            let
                _ =
                    if False then
                        Debug.log "Failed to scroll, will retry after a user message" id
                    else
                        ""
            in
                ( { model | retryScroll = Just id }, Cmd.none )


scrollToHashIfDifferent : Maybe LinkMonitor.Id -> Navigation.Location -> Cmd (Msg userMsg route)
scrollToHashIfDifferent scrolledId location =
    if scrolledId /= Just (idFromHash location) then
        scrollToHash location
    else
        Cmd.none


scrollToHash : Navigation.Location -> Cmd (Msg userMsg route)
scrollToHash location =
    scrollToId (idFromHash location)


scrollToId : LinkMonitor.Id -> Cmd (Msg userMsg route)
scrollToId id =
    if not (String.isEmpty id) then
        LinkMonitor.scrollTo id
            |> Task.map (always <| ScrollSuccess id)
            |> Task.onError
                (\(LinkMonitor.NotFound id) -> Task.succeed (ScrollFailure id))
            |> Task.perform identity
    else
        Task.succeed (ScrollSuccess "")
            |> Task.perform identity



-- VIEW


view : App model route msg -> Model model route -> Html (Msg msg route)
view app model =
    Html.Lazy.lazy3 app.view (href app) model.userModel model.route
        |> Html.Lazy.lazy (Html.map UserMsg)



-- SUBSCRIPTIONS


isNavigationClick : Json.Decoder ()
isNavigationClick =
    let
        expect : a -> Json.Decoder a -> Json.Decoder ()
        expect a decoder =
            decoder
                |> Json.andThen
                    (\b ->
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
            [ expect "true" <| Json.at [ "target", "dataset", "elmRouter" ] Json.string
            , expect 0 <| Json.field "button" Json.int
            , expect False <| Json.field "altKey" Json.bool
            , expect False <| Json.field "ctrlKey" Json.bool
            , expect False <| Json.field "altKey" Json.bool
            , expect False <| Json.field "metaKey" Json.bool
            , expect False <| Json.field "shiftKey" Json.bool
            ]


subscriptions : App model route msg -> Model model route -> Sub (Msg msg route)
subscriptions app model =
    Sub.batch
        [ LinkMonitor.clicks isNavigationClick LinkClick
        , app.subscriptions model.userModel |> Sub.map UserMsg
        ]



-- HELPERS


{-| Your `view` function should use this function to generate links (essentially
`href` attributes) from routes. These will look like ordinary links to the browser.
In general, the browser can display the target (unlike "links" added using `onClick`
handlers), and all usual click actions (right-click, modifier+click etc.) will work
as expected. Only clicks that cause navigation (e.g. left click without modifiers)
will be caught by `UrlRouter` and passed along to your app as a new route so that
they don't cause a page reload.

TODO: example
-}
type alias Href route msg =
    route -> List (Attribute msg)


{-| Generate an `href` from a `route`. See the description of `Href`.
-}
href : App model route msg -> Href route msg
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


{-| Switch to the given route. We also need the old route so we can decide
whether we want a new entry or modify the current one.
-}
changeRoute : App model route msg -> route -> ChangeRoute route mgs_
changeRoute app oldRoute newRoute =
    let
        url : String
        url =
            case UrlParser.reverse app.router newRoute of
                Nothing ->
                    Debug.crash <| reverseError newRoute

                Just segment ->
                    UrlSegment.toPath segment

        method : String -> Cmd a
        method =
            if app.newHistoryEntry oldRoute newRoute then
                Navigation.newUrl
            else
                Navigation.modifyUrl
    in
        -- TODO: should we ignore if the routes are equal?
        method url


{-| Turn a location into a route. We also return a command in case we need to `normalize`
the browser url.
-}
getRoute : App model route msg -> Location -> GetRoute msg route
getRoute app location =
    let
        getPath : Location -> String
        getPath location =
            location.pathname ++ location.search ++ location.hash
    in
        case UrlParser.parse app.router (UrlSegment.fromLocationPath location) of
            Nothing ->
                NotFound

            Just route ->
                case UrlParser.reverse app.router route of
                    Nothing ->
                        Debug.crash <| faultyIsoReverse route (getPath location)

                    Just normalizedSegment ->
                        if getPath location == UrlSegment.toPath normalizedSegment then
                            -- Browser url is already normalized
                            Found route
                        else
                            -- We need to normalize the browser url
                            FoundWithRedirect route (Navigation.modifyUrl <| UrlSegment.toPath normalizedSegment)


type GetRoute userMsg route
    = NotFound
    | Found route
    | FoundWithRedirect route (Cmd (Msg userMsg route))


idFromHash : Location -> String
idFromHash location =
    String.dropLeft 1 location.hash



-- ERROR MESSAGES


reverseError : route -> String
reverseError route =
    """
Your router failed to reverse a route provided by you.
This could indicate a problem with your router or partial isomorphisms.
The problematic route is: """ ++ toString route


faultyIsoParse : String -> String
faultyIsoParse url =
    """
Your router failed to parse a route that it created itself (by reversing a route provided to `href`).
This should never happen, and usually indicates a problem with your partial isomorphims.
The route was generated from: """ ++ url


faultyIsoReverse : route -> String -> String
faultyIsoReverse route url =
    """
Your router failed to reverse a route that it created itself.
This should never happen, and usually indicates a problem with your partial isomorphims.
  The route was: """
        ++ toString route
        ++ "\n"
        ++ "And it was generated from: "
        ++ url
