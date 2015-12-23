module Main where

import StartApp
import Effects
import Signal exposing (forwardTo)
import Task
import Http

import Html

import Ui.App

import Debug exposing (log)

import VideoLibrary.Types exposing (..)

-- Main entry point
type alias Model =
  { app: Ui.App.Model
  , items: List Item
  }

type Action
  = App Ui.App.Action
  | Loaded (Maybe (List Item))

init : Model
init =
  { app = Ui.App.init "Video Library"
  , items = []
  }

view: Signal.Address Action -> Model -> Html.Html
view address model =
  Ui.App.view (forwardTo address App) model.app
    []

update: Action -> Model -> Model
update action model =
  case action of
    App act ->
      { model | app = Ui.App.update act model.app }
    Loaded (Just items)->
      { model | items = log "a" items }
    Loaded Nothing ->
      { model | items = log "a" [] }

fxNone : Model -> (Model, Effects.Effects action)
fxNone model =
  (model, Effects.none)

app =
  StartApp.start { init = (log "init" init, fetchData Loaded)
                 , view = view
                 , update = (\action model -> fxNone (update action model))
                 , inputs = [] }

main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
