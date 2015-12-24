module Main where

import StartApp
import Effects
import Signal exposing (forwardTo)
import Task
import Http
import Hop
import Dict

import Html.Attributes exposing (style, src, controls, classList)
import Html.Events exposing (onClick)
import Html exposing (node, text)

import Ui.Container
import Ui.App
import Ui

import Debug exposing (log)

import VideoLibrary.Types exposing (..)

-- Main entry point
type alias Model =
  { app: Ui.App.Model
  , items: List Item
  , item: Maybe Item
  }

type Action
  = App Ui.App.Action
  | Loaded (Maybe (List Item))
  | HopAction Hop.Action
  | ShowRoot Hop.Payload
  | ShowItem Hop.Payload
  | NavigateTo String

init : Model
init =
  { app = Ui.App.init "Video Library"
  , item = Nothing
  , items = []
  }

routes : List (String, Hop.Payload -> Action)
routes =
  [ ("/", ShowRoot)
  , ("/item/:itemId", ShowItem)
  ]

router : Hop.Router Action
router =
  Hop.new
    { routes = routes
    , notFoundAction = ShowRoot
    }

breadcrumbs : Signal.Address a -> Html.Html -> List (String, a) -> Html.Html
breadcrumbs address separator items =
  let
    renderItem (label, action) =
      node "ui-breadcrumb" [onClick address action]
        [node "span" [] [text label]]
  in
    node "ui-breadcrumbs" []
      (List.map renderItem items
      |> List.intersperse separator)

renderItem : Signal.Address Action -> Item -> Html.Html
renderItem address item =
  let
    (url, kind) =
      case item of
        VideoNode video ->
          ("/item/" ++ video.id, "video")
        FolderNode folder ->
          ("/item/" ++ folder.id, "folder")
  in
    node "video-library-item"
      [ onClick address (NavigateTo url)
      , classList [(kind, True)]
      ]
      [ node "video-library-item-image" [style [("background-image", "url(\"" ++ (itemImage item) ++ "\")")]] []
      , node "div" [] [text (itemName item)]
      ]

view: Signal.Address Action -> Model -> Html.Html
view address model =
  let
    path item =
      if item.id == "" then []
      else [(item.name, NavigateTo ("/item/" ++ item.id))]

    (child, breadcrumbItems, isVideo) =
      case model.item of
        Just item ->
          case item of
            VideoNode video ->
              (node "video-library-video" []
                [node "video" [src video.url, controls True] []],path video,True)
            FolderNode folder ->
              (node "video-library-folder" []
                (List.map (\item -> renderItem address item) folder.items), path folder, False)
        _ -> (node "div" [] [], [], False)
  in
    Ui.App.view (forwardTo address App) model.app
      [ node "video-library" [classList [("video", isVideo)]]
        [ Ui.Container.view { align = "stretch"
                              , direction = "column"
                              , compact = True } []
              [ Ui.header []
                [ Ui.headerTitle [] [text "My Video Library"]]
              , breadcrumbs address (node "span" [] [text "/"]) ([("Library", NavigateTo "")] ++ breadcrumbItems)
              , child
              ]
        ]
      ]

getItemId : Hop.Payload -> String
getItemId payload =
  payload.params
    |> Dict.get "itemId"
    |> Maybe.withDefault ""

update: Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    NavigateTo path ->
      (model, Effects.map HopAction (Hop.navigateTo path))
    App act ->
      { model | app = Ui.App.update act model.app }
        |> fxNone
    ShowRoot payload ->
      showRoot model
    ShowItem payload ->
      { model | item = findItemByID (getItemId payload) model.items }
        |> fxNone
    Loaded (Just items)->
      { model | items = log "a" items }
        |> showRoot
    Loaded Nothing ->
      { model | items = log "a" []
              , item = Nothing }
        |> fxNone
    _ -> fxNone model

showRoot : Model -> (Model, Effects.Effects Action)
showRoot model =
  { model | item = Just (FolderNode { name = "ROOT", items = model.items, id = "" }) }
    |> fxNone

fxNone : Model -> (Model, Effects.Effects action)
fxNone model =
  (model, Effects.none)

app =
  StartApp.start { init = (log "init" init, fetchData Loaded)
                 , view = view
                 , update = update
                 , inputs = [Signal.dropRepeats router.signal] }

main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
