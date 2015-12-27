module Main where

import StartApp
import Effects
import Signal exposing (forwardTo)
import Task
import Http
import Hop
import Dict
import Keyboard

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
  , folder: Maybe Folder
  , video: Maybe Video
  , routerPayload : Hop.Payload
  }

type Action
  = App Ui.App.Action
  | Loaded (Maybe (List Item))
  | HopAction Hop.Action
  | HandleChange Hop.Payload
  | NavigateTo (Dict.Dict String String)
  | EscIsDown Bool

init : Model
init =
  { app = Ui.App.init "Video Library"
  , routerPayload = router.payload
  , folder = Nothing
  , video = Nothing
  , items = []
  }

routes : List (String, Hop.Payload -> Action)
routes =
  [ ("/", HandleChange) ]

router : Hop.Router Action
router =
  Hop.new
    { routes = routes
    , notFoundAction = HandleChange
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

createQuery : Model -> Dict.Dict String String
createQuery model =
  let
    folder =
      Maybe.map (\item -> [("folderId", item.id)]) model.folder
        |> Maybe.withDefault []
    video =
      Maybe.map (\item -> [("videoId", item.id)]) model.video
        |> Maybe.withDefault []
  in
    Dict.fromList (folder ++ video)

renderItem : Signal.Address Action -> Model -> Item -> Html.Html
renderItem address model item =
  let
    (query, kind) =
      case item of
        VideoNode video ->
          (createQuery { model | video = Just video }, "video")
        FolderNode folder ->
          (createQuery { model | folder = Just folder }, "folder")
  in
    node "video-library-item"
      [ onClick address (NavigateTo query)
      , classList [(kind, True)]
      ]
      [ node "video-library-item-image" [style [("background-image", "url(\"" ++ (itemImage item) ++ "\")")]] []
      , node "div" [] [text (itemName item)]
      ]

view: Signal.Address Action -> Model -> Html.Html
view address model =
  let
    path item =
      ((itemPath item.id [] (rootNode model)) ++ [item])
      |> List.map (\(folder) -> (folder.name, NavigateTo (createQuery { model | folder = Just folder })))

    (child, breadcrumbItems, isVideo) =
      case model.folder of
        Just folder ->
          (node "video-library-folder" []
            (List.map (\item -> renderItem address model item) folder.items), path folder, False)
        _ -> (node "div" [] [], [], False)

    videoPlayer =
      case model.video of
        Just video ->
          node "video-library-video" []
            [node "video" [src video.url, controls True] []]
        Nothing -> text ""
  in
    Ui.App.view (forwardTo address App) model.app
      [ node "video-library" [classList [("video", isVideo)]]
        [ Ui.Container.view { align = "stretch"
                              , direction = "column"
                              , compact = True } []
              [ Ui.header []
                [ Ui.headerTitle [] [text "My Video Library"]]
              , breadcrumbs address (node "span" [] [text "/"]) breadcrumbItems
              , child
              ]
        ]
      , videoPlayer
      ]

getParam : String -> Hop.Payload -> String
getParam key payload =
  payload.params
    |> Dict.get key
    |> Maybe.withDefault ""

update: Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    NavigateTo query ->
      (model, Effects.map HopAction (Hop.setQuery model.routerPayload.url query))
    App act ->
      { model | app = Ui.App.update act model.app }
        |> fxNone
    HandleChange payload ->
      let
        folder =
          Maybe.oneOf
            [ maybeAsFolder (findItemByID (getParam "folderId" payload) model.items)
            , Just (rootNode model)
            ]
      in
        { model | routerPayload = payload
                , folder = folder
                , video = maybeAsVideo (findItemByID (getParam "videoId" payload) model.items)
        } |> fxNone
    Loaded (Just items)->
      { model | items = log "a" items }
        |> showRoot
    EscIsDown isDown ->
      let
        query =
          createQuery { model | video = Nothing}
      in
        if isDown then
          (model, Effects.map HopAction (Hop.setQuery model.routerPayload.url query))
        else
          fxNone model
    Loaded Nothing ->
      { model | items = log "a" []
              , folder = Nothing
              , video = Nothing }
        |> fxNone
    _ -> fxNone model

rootNode : Model -> Folder
rootNode model =
  { name = "Library", items = model.items, id = "" }

showRoot : Model -> (Model, Effects.Effects Action)
showRoot model =
  { model | folder = Just (rootNode model) }
    |> fxNone

fxNone : Model -> (Model, Effects.Effects action)
fxNone model =
  (model, Effects.none)

app =
  StartApp.start { init = (log "init" init, fetchData Loaded)
                 , view = view
                 , update = update
                 , inputs = [ Signal.dropRepeats router.signal
                            , Signal.map EscIsDown (Keyboard.isDown 27) ] }

main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
