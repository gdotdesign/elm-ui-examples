module Main where

import StartApp
import Effects
import Signal exposing (forwardTo)
import Task
import Http
import Hop
import Dict
import Keyboard
import List.Extra
import Json.Encode as J
import Mouse
import String

import Html.Attributes exposing (style, src, controls, classList)
import Html.Events exposing (onClick)
import Html exposing (node, text)

import Ui.NotificationCenter
import Ui.DropdownMenu
import Ui.Container
import Ui.Button
import Ui.App
import Ui

import Debug exposing (log)

import VideoLibrary.Types exposing (..)
import VideoLibrary.Components.FolderView as FolderView
import VideoLibrary.Components.Item as Item
import VideoLibrary.Components.Modal as Modal

import VideoLibrary.Forms.Video as VideoForm
import VideoLibrary.Forms.Folder as FolderForm

-- Main entry point
type alias Model =
  { app: Ui.App.Model
  , video: Maybe Video
  , routerPayload : Hop.Payload
  , fabMenu: Ui.DropdownMenu.Model
  , folds : List Folder
  , vids : List Video
  , notifications : Ui.NotificationCenter.Model
  , folderView : FolderView.Model
  , folderModal : Modal.Model FolderForm.Model Folder FolderForm.Action
  , videoModal: Modal.Model VideoForm.Model Video VideoForm.Action
  }

type Action
  -- Request results
  = FolderContentsLoaded (Result String FolderContents)
  | VideoLoaded (Result String Video)
  | VideoSaved (Result String Video)
  | FolderSaved (Result String Folder)
  | Refresh (Result String J.Value)
  -- Navigation
  | NavigateTo (Dict.Dict String String)
  | HandleChange Hop.Payload
  | HopAction Hop.Action
  -- Components
  | Notifications Ui.NotificationCenter.Action

  | FabMenu Ui.DropdownMenu.Action
  | FolderView FolderView.Action
  | App Ui.App.Action
  -- Lifecycle
  | CreateOrPatchFolder
  | CreateOrPatchVideo
  | MouseIsDown Bool
  | EscIsDown Bool
  | DeleteFolder Int
  | DeleteVideo Int
  | OpenVideoModal (Maybe Int)
  | OpenFolderModal (Maybe Int)
  | NoOp
  -- Modals
  | FolderModal (Modal.Action FolderForm.Action)
  | VideoModal (Modal.Action VideoForm.Action)

init : Model
init =
  let
    fabMenu = Ui.DropdownMenu.init
  in
    { app = Ui.App.init "Video Library"
    , routerPayload = router.payload
    , notifications = Ui.NotificationCenter.init 4000 320
    , video = Nothing
    , folderView = FolderView.init [] []
    , vids = []
    , folds = []
    , fabMenu = { fabMenu | offsetY = 10
                          , favoredSides = { horizontal = "left"
                                           , vertical = "top"
                                           }
                }
    , folderModal = Modal.init { init = FolderForm.init
                               , update = FolderForm.update
                               , asParams = FolderForm.asParams
                               , fromEntity = FolderForm.fromFolder
                               , isValid = FolderForm.isValid
                               , isNew = FolderForm.isNew
                               }
    , videoModal = Modal.init { init = VideoForm.init
                              , update = VideoForm.update
                              , asParams = VideoForm.asParams
                              , fromEntity = VideoForm.fromVideo
                              , isValid = VideoForm.isValid
                              , isNew = VideoForm.isNew
                              }
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

createQuery : Maybe Int -> Maybe Int -> Hop.Payload -> Dict.Dict String String
createQuery folderId videoId payload =
  Dict.fromList
    [ ("folderId", toString (Maybe.withDefault (getParam "folderId" payload) folderId))
    , ("videoId", toString (Maybe.withDefault (getParam "videoId" payload) videoId))
    ]

va address base model =
  let
    query = createQuery Nothing (Just model.id) base.routerPayload
  in
    { onClick = onClick address (NavigateTo query)
    , onDelete = onClick address (DeleteVideo model.id)
    , onEdit = onClick address (OpenVideoModal (Just model.id))
    }

fa address base model =
  let
    query = createQuery (Just model.id) Nothing base.routerPayload
  in
    { onClick = onClick address (NavigateTo query)
    , onDelete = onClick address (DeleteFolder model.id)
    , onEdit = onClick address (OpenFolderModal (Just model.id))
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

view: Signal.Address Action -> Model -> Html.Html
view address model =
  let
    videoPlayer =
      case model.video of
        Just video ->
          node "video-library-video" []
            [ Ui.Container.row []
              [ node "strong" [] [text video.name]
              , Ui.icon "close" True [onClick address (EscIsDown True)]
              ]
            , node "video" [src video.url, controls True] []]
        Nothing -> text ""
  in
    Ui.App.view (forwardTo address App) model.app
      [ Ui.NotificationCenter.view (forwardTo address Notifications) model.notifications
      , Modal.view (forwardTo address VideoModal)
          { address = address
          , action = CreateOrPatchVideo
          , view = VideoForm.view
          , newTexts = ("Add Video", "Add")
          , saveTexts = ("Edit Video", "Save")
          }
          model.videoModal
      , Modal.view (forwardTo address FolderModal)
          { address = address
          , action = CreateOrPatchFolder
          , view = FolderForm.view
          , newTexts = ("Add Folder", "Add")
          , saveTexts = ("Edit Folder", "Save")
          }
          model.folderModal
      , node "video-library" []
        [ Ui.Container.view { align = "stretch"
                            , direction = "column"
                            , compact = True } []
              [ Ui.header []
                [ Ui.headerTitle [] [text "My Video Library"] ]
              -- , breadcrumbs address (node "span" [] [text "/"]) breadcrumbItems
              , FolderView.view
                (forwardTo address FolderView)
                { videoActions = fa address model
                , folderActions = va address model
                }
                model.folderView
              ]
        ]
      , videoPlayer
      , Ui.DropdownMenu.view
        (forwardTo address FabMenu)
        (Ui.fab "plus" [])
        [ Item.menuItem "android-film" "Add Video" (onClick address (OpenVideoModal Nothing))
        , Item.menuItem "folder" "Add Folder" (onClick address (OpenFolderModal Nothing))
        ]
        model.fabMenu
      ]

getParam : String -> Hop.Payload -> Int
getParam key payload =
  payload.params
    |> Dict.get key
    |> Maybe.withDefault ""
    |> String.toInt
    |> Result.withDefault 0

loadFolderContents id =
  fetchFolderContents id FolderContentsLoaded

refresh : Model -> (Model, Effects.Effects Action)
refresh model =
  let
    folderId = getParam "folderId" model.routerPayload
  in
    (closeModals model, loadFolderContents folderId)

update: Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    NavigateTo query ->
      (model, Effects.map HopAction (Hop.addQuery model.routerPayload.url query))

    Notifications act ->
      let
        (notifications, effect) =
          Ui.NotificationCenter.update act model.notifications
      in
        ({ model | notifications = notifications }, Effects.map Notifications effect)

    App act ->
      { model | app = Ui.App.update act model.app }
        |> fxNone
    VideoModal act ->
      { model | videoModal = Modal.update act model.videoModal }
        |> fxNone
    FolderModal act ->
      { model | folderModal = Modal.update act model.folderModal }
        |> fxNone

    FolderView act ->
      { model | folderView = FolderView.update act model.folderView }
        |> fxNone

    DeleteFolder id ->
      (closeDropdowns False model, deleteFolder id Refresh)
    DeleteVideo id ->
      (closeDropdowns False model, deleteVideo id Refresh)

    Refresh (Ok _) ->
      refresh model
    VideoSaved (Ok video) ->
      refresh model
    FolderSaved (Ok video) ->
      refresh model

    CreateOrPatchFolder ->
      let
        params = Modal.asParams model.folderModal
        id = Maybe.withDefault 0 model.folderModal.form.id
        folderId = getParam "folderId" model.routerPayload
        params' = params ++ [("folderId", J.int folderId)]
      in
        if Modal.isNew model.folderModal then
          (model, createFolder params' FolderSaved)
        else
          (model, patchFolder id params' FolderSaved)
    CreateOrPatchVideo ->
      let
        params = Modal.asParams model.videoModal
        id = Maybe.withDefault 0 model.videoModal.form.id
        folderId = getParam "folderId" model.routerPayload
        params' = params ++ [("folderId", J.int folderId)]
      in
        if Modal.isNew model.videoModal then
          (model, createVideo params' VideoSaved)
        else
          (model, patchVideo id params' VideoSaved)
    OpenFolderModal Nothing ->
      { model | folderModal = Modal.open model.folderModal }
        |> closeDropdowns False
        |> fxNone
    OpenFolderModal (Just folderId) ->
      let
        folder = List.Extra.find (\item -> item.id == folderId) model.folds
      in
        case folder of
          Just fold ->
            { model | folderModal = Modal.openWithEntity fold model.folderModal }
              |> closeDropdowns False
              |> fxNone
          _ -> fxNone model
    OpenVideoModal Nothing ->
      { model | videoModal = Modal.open model.videoModal }
        |> closeDropdowns False
        |> fxNone
    OpenVideoModal (Just videoId) ->
      let
        video = List.Extra.find (\item -> item.id == videoId) model.vids
      in
        case video of
          Just vid ->
            { model | videoModal = Modal.openWithEntity vid model.videoModal }
              |> closeDropdowns False
              |> fxNone
          _ -> fxNone model
    FabMenu act ->
      { model | fabMenu = Ui.DropdownMenu.update act model.fabMenu }
        |> fxNone
    HandleChange payload ->
      let
        folderId = getParam "folderId" payload
        currentFolderId = getParam "folderId" model.routerPayload
        videoId = getParam "videoId" payload
        currentVideoId = getParam "videoId" model.routerPayload

        folderEffects =
          if folderId /= currentFolderId then (loadFolderContents folderId)
          else Effects.none

        videoEffects =
          if currentVideoId == 0 && videoId /= 0 then (fetchVideo videoId VideoLoaded)
          else Effects.none

        updatedModel =
          { model | routerPayload = payload
                  , video = if videoId == 0 then Nothing else model.video }
      in
        (updatedModel, Effects.batch [folderEffects, videoEffects])
    VideoLoaded (Ok video) ->
      { model | video = Just video }
        |> fxNone
    FolderContentsLoaded (Err message) ->
      notify message model
    FolderContentsLoaded (Ok contents) ->
      { model | folderView = FolderView.init contents.folders contents.videos
              , vids = contents.videos
              , folds = contents.folders }
        |> fxNone
    MouseIsDown pressed ->
      closeDropdowns pressed model
        |> fxNone



    EscIsDown isDown ->
      let
        query =
          createQuery Nothing (Just 0) model.routerPayload
      in
        if isDown then
          (model, Effects.map HopAction (Hop.addQuery model.routerPayload.url query))
        else
          model
          |> closeModals
          |> closeDropdowns isDown
          |> fxNone
    _ -> fxNone model

notify message model =
  let
    (notifications, effect) =
      Ui.NotificationCenter.notify (text message) model.notifications
  in
    ({ model | notifications = notifications }, Effects.map Notifications effect)

closeDropdowns pressed model =
  { model | folderView = FolderView.handleClick pressed model.folderView
          , fabMenu = Ui.DropdownMenu.handleClick pressed model.fabMenu
          }

closeModals model =
  { model | videoModal = Modal.close model.videoModal
          , folderModal = Modal.close model.folderModal }

fxNone : Model -> (Model, Effects.Effects action)
fxNone model =
  (model, Effects.none)

app =
  StartApp.start { init = (init, loadFolderContents 0)
                 , view = view
                 , update = update
                 , inputs = [ Signal.dropRepeats router.signal
                            , Signal.map EscIsDown (Keyboard.isDown 27)
                            , Signal.map MouseIsDown Mouse.isDown] }

main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
