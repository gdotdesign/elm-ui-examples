module Main where

import StartApp
import Effects
import Signal exposing (forwardTo)
import Task
import Http
import Hop
import Dict

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
  | NavigateTo String

routes : List (String, Hop.Payload -> Action)
routes =
  [ ("/", ShowRoot)
  , ("/folders/:folderId", ShowFolder)
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
        FolderNode folder -> [onClick address (NavigateTo ("/folders/" ++ folder.id))]
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

getFolderId : Hop.Payload -> String
getFolderId payload =
  payload.params
    |> Dict.get "folderId"
    |> Maybe.withDefault ""

update: Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    NavigateTo path ->
      (model, Effects.map HopAction (Hop.navigateTo path))
    App act ->
      { model | app = Ui.App.update act model.app }
        |> fxNone
    Select folder ->
      { model | folder = Just folder }
        |> fxNone
    ShowRoot payload ->
      showRoot model
    ShowFolder payload ->
      { model | folder = findFolderById (getFolderId payload) model.items }
        |> fxNone
    Loaded (Just items)->
      { model | items = log "a" items }
        |> showRoot
    Loaded Nothing ->
      { model | items = log "a" []
              , folder = Nothing }
        |> fxNone
    _ -> fxNone model

showRoot : Model -> (Model, Effects.Effects Action)
showRoot model =
  { model | folder = Just { name = "ROOT", items = model.items, id = "root" } }
    |> fxNone

fxNone : Model -> (Model, Effects.Effects action)
fxNone model =
  (model, Effects.none)

app =
  StartApp.start { init = (log "init" init, fetchData Loaded)
                 , view = view
                 , update = update
                 , inputs = [router.signal] }

main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
