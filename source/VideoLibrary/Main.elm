module Main where

import StartApp
import Effects
import Signal exposing (forwardTo)
import Task
import Http
import Hop

import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html exposing (node, text)

import Ui.App
import Ui

import Debug exposing (log)

import VideoLibrary.Types exposing (..)

-- Main entry point
type alias Model =
  { app: Ui.App.Model
  , items: List Item
  , folder: Maybe Folder
  }

type Action
  = App Ui.App.Action
  | Loaded (Maybe (List Item))
  | Select Folder
  | HopAction Hop.Action
  | ShowRoot Hop.Payload
  | ShowFolder Hop.Payload

routes : List (String, Hop.Payload -> Action)
routes =
  [ ("/", ShowRoot)
  ,  ("/folder/:id", ShowFolder)
  ]

router : Hop.Router Action
router =
  Hop.new
    { routes = routes
    , notFoundAction = ShowRoot
    }

init : Model
init =
  { app = Ui.App.init "Video Library"
  , folder = Nothing
  , items = []
  }

renderItem : Signal.Address Action -> Item -> Html.Html
renderItem address item =
  let
    action =
      case item of
        VideoNode video -> []
        FolderNode folder -> [onClick address (Select folder)]
  in
    node "video-library-item" action
      [ node "video-library-item-image" [style [("background-image", "url(" ++ (itemImage item) ++ ")")]] []
      , node "div" [] [text (itemName item)]
      ]

view: Signal.Address Action -> Model -> Html.Html
view address model =
  let
    items =
      case model.folder of
        Just folder -> List.map (\item -> renderItem address item) folder.items
        _ -> []
  in
    Ui.App.view (forwardTo address App) model.app
      [ node "video-library-folder" [] items ]

update: Action -> Model -> Model
update action model =
  case action of
    App act ->
      { model | app = Ui.App.update act model.app }
    Select folder ->
      { model | folder = Just folder }
    Loaded (Just items)->
      { model | items = log "a" items
              , folder = Just { name = "ROOT", items = items, id = "root" } }
    Loaded Nothing ->
      { model | items = log "a" []
              , folder = Nothing }
    _ -> model

fxNone : Model -> (Model, Effects.Effects action)
fxNone model =
  (model, Effects.none)

app =
  StartApp.start { init = (log "init" init, fetchData Loaded)
                 , view = view
                 , update = (\action model -> fxNone (update action model))
                 , inputs = [router.signal] }

main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
