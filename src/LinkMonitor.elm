effect module LinkMonitor where { subscription = MySub } exposing
  ( Url
  , clicks
  )

{-| A generic monitor for click events on HTML nodes that have an href attribute.

TODO: improve this.
It is intended to be used as monitor for clicks to HTML nodes that have an `href`
attribute and are marked with a custom HTML5 `data` attribute. Navigation-causing
clicks will be cancelled and reported. However, clicks that do not cause navigation
(such as right-clicks and modified left-clicks) will work as expected and you won't
get notifications with those.

# Link Monitor
@docs clicks

-}

import Dict
import Process
import Task exposing (Task)

import Json.Decode as Json

import Native.LinkMonitor


-- LINK EVENTS

type alias Url =
  String


{-| Subscribe to click events on HTML nodes. You provide a decoder that should
succeed on events you want to keep. If the decoder succeeds, you get a message
containing the value of the node's href attribute and the default behavior of
the event is canceled (e.g. clicking on a link won't cause navigation). Otherwise,
the event causes the normal behavior and you are not notified. For example, you
might say like this to capture all clicks:

    type Msg = Navigate String | ...

    subscriptions model =
      clicks (Json.Decode.succeed ()) Navigate
-}
clicks : Json.Decoder () -> (Url -> msg) -> Sub msg
clicks decoder tagger =
  subscription (Clicks decoder tagger)


{-| Add an event handler on the `document` that monitors `click` events. If
the provided decoder succeeds, the default action of the event is cancelled.

The resulting task will never end, and when you kill the process it is on,
it will detach the relevant JavaScript event listener.
-}
onClick : Json.Decoder a -> (a -> Task Never ()) -> Task Never Never
onClick =
  Native.LinkMonitor.onClick



-- SUBSCRIPTIONS

type MySub msg
  = Clicks (Json.Decoder ()) (Url -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
  case sub of
    Clicks decoder tagger ->
      Clicks decoder (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
  { subs : List (Monitor msg) }


type alias Monitor msg =
  { decoder : Json.Decoder ()
  , tagger : Url -> msg
  , pid : Process.Id
  }


-- EFFECT MANAGER


init : Task Never (State msg)
init =
  Task.succeed { subs = [] }


type alias Msg =
  { event : Json.Value }


(&>) t1 t2 =
  Task.andThen (always t2) t1


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
  let
    cleanupOld : Task Never ()
    cleanupOld =
      List.map (.pid >> Process.kill) oldState.subs
        |> Task.sequence
        |> Task.andThen (always <| Task.succeed ())

    monitor : MySub msg -> Task Never (Monitor msg)
    monitor (Clicks decoder tagger) =
      let
        handler : Task Never Never
        handler =
          onClick
            (decoder |> Json.andThen (always href) |> Json.andThen (always Json.value))
            (Msg >> Platform.sendToSelf router)
      in
        Process.spawn handler |> Task.andThen (\pid -> Task.succeed <| Monitor decoder tagger pid)

    startNew : Task Never (State msg)
    startNew =
      List.map monitor newSubs
        |> Task.sequence
        |> Task.andThen (State >> Task.succeed)
  in
    cleanupOld &> startNew


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router {event} state =
  let
    -- Send to app if the decoder associated with the monitor succeeds
    maybeSend : Monitor msg -> Task Never ()
    maybeSend {decoder, tagger} =
      case Json.decodeValue (decoder |> Json.andThen (always href)) event of
        Result.Ok url ->
          Platform.sendToApp router (tagger url)

        Result.Err _ ->
          Task.succeed ()
  in
    Task.sequence (List.map maybeSend state.subs)
      &> Task.succeed state


-- Decode the href attribute of an event
href : Json.Decoder Url
href =
  Json.at ["target", "href"] Json.string

