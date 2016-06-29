module Components.Player exposing (..)

import Html.Attributes exposing (controls, src)
import Html.Events.Extra exposing (onError)
import Html.Events exposing (onClick)
import Html exposing (node, text)

import Types exposing (Video)

import Ui.Helpers.Emitter as Emitter
import Ui.Container
import Ui

type alias Model =
  { video : Maybe Video }

type Msg
  = Close
  | Open Int
  | Opened Video
  | Error String

init : Model
init =
  { video = Nothing }

subscriptions : Sub Msg
subscriptions =
  Emitter.listenInt "open-video" Open

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Error message ->
      (model, Emitter.sendString "errors" message)

    Opened video ->
      ({ model | video = Just video }, Cmd.none)

    Close ->
      ({ model | video = Nothing }, Cmd.none)

    Open id ->
      (model, Types.fetchVideo id Error Opened)

view : Model -> Html.Html Msg
view model =
  case model.video of
    Just video ->
      node "video-library-video" []
        [ Ui.Container.row []
          [ node "strong" [] [text video.name]
          , Ui.icon "close" True [onClick Close]
          ]
        , node "video" [src video.url, onError (Error "Could not play the given video!"), controls True] []
        ]
    Nothing -> text ""
