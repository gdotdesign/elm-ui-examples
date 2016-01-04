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

import Ui.DropdownMenu
import Ui.Container
import Ui.Button
import Ui.App
import Ui

import Debug exposing (log)

import VideoLibrary.Types exposing (..)
import VideoLibrary.Components.Folder as FolderComponent
import VideoLibrary.VideoModal as VideoModal
import VideoLibrary.FolderModal as FolderModal

-- Main entry point
type alias Model =
  { app: Ui.App.Model
  , folders: List FolderComponent.Model
  , videos : List FolderComponent.Model
  , video: Maybe Video
  , routerPayload : Hop.Payload
  , fabMenu: Ui.DropdownMenu.Model
  , videoModal: VideoModal.Model
  , folderModal : FolderModal.Model
  , folds : List Folder
  , vids : List Video
  }

type Action
  -- Request results
  = FolderContentsLoaded (Result String FolderContents)
  | VideoLoaded (Result String Video)
  | VideoSaved (Result String Video)
  | FolderSaved (Result String Folder)
  -- Navigation
  | NavigateTo (Dict.Dict String String)
  | HandleChange Hop.Payload
  | HopAction Hop.Action
  -- Components
  | FolderAction Int FolderComponent.Action
  | VideoAction Int FolderComponent.Action
  | FabMenu Ui.DropdownMenu.Action
  | FolderModal FolderModal.Action
  | VideoModal VideoModal.Action
  | App Ui.App.Action
  -- Lifecycle
  | CreateOrPatchFolder
  | CreateOrPatchVideo
  | MouseIsDown Bool
  | EscIsDown Bool
  | OpenVideoModal (Maybe Int)
  | OpenFolderModal (Maybe Int)
  | NoOp

init : Model
init =
  let
    fabMenu = Ui.DropdownMenu.init
  in
    { app = Ui.App.init "Video Library"
    , routerPayload = router.payload
    , video = Nothing
    , folders = []
    , videos = []
    , vids = []
    , folds = []
    , videoModal = VideoModal.init
    , folderModal = FolderModal.init
    , fabMenu = { fabMenu | offsetY = 10
                          , favoredSides = { horizontal = "left"
                                           , vertical = "top"
                                           }
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

renderVideo address base model =
  let
    query = createQuery Nothing (Just model.id) base.routerPayload
  in
    FolderComponent.view
      (forwardTo address (VideoAction model.id))
      { onClick = onClick address (NavigateTo query)
      , onDelete = onClick address NoOp
      , onEdit = onClick address (OpenVideoModal (Just model.id))
      }
      model

renderFolder address base model =
  let
    query = createQuery (Just model.id) Nothing base.routerPayload
  in
    FolderComponent.view
      (forwardTo address (FolderAction model.id))
      { onClick = onClick address (NavigateTo query)
      , onDelete = onClick address NoOp
      , onEdit = onClick address (OpenFolderModal (Just model.id))
      }
      model

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
      [ VideoModal.view (forwardTo address VideoModal)
          { address = address
          , action = CreateOrPatchVideo
          }
          model.videoModal
      , FolderModal.view (forwardTo address FolderModal)
          { address = address
          , action = CreateOrPatchFolder }
          model.folderModal
      , node "video-library" []
        [ Ui.Container.view { align = "stretch"
                              , direction = "column"
                              , compact = True } []
              [ Ui.header []
                [ Ui.headerTitle [] [text "My Video Library"]]
              -- , breadcrumbs address (node "span" [] [text "/"]) breadcrumbItems
              , (node "video-library-folder" []
                  ((List.map (\item -> renderFolder address model item) model.folders)
                    ++
                    (List.map (\item -> renderVideo address model item) model.videos)
                ))
              ]
        ]
      , videoPlayer
      , Ui.DropdownMenu.view
        (forwardTo address FabMenu)
        (Ui.fab "plus" [])
        [ FolderComponent.menuItem "android-film" "Add Video" (onClick address (OpenVideoModal Nothing))
        , FolderComponent.menuItem "folder" "Add Folder" (onClick address (OpenFolderModal Nothing))
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

update: Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    NavigateTo query ->
      (model, Effects.map HopAction (Hop.addQuery model.routerPayload.url query))
    App act ->
      { model | app = Ui.App.update act model.app }
        |> fxNone
    VideoModal act ->
      { model | videoModal = VideoModal.update act model.videoModal }
        |> fxNone
    FolderModal act ->
      { model | folderModal = FolderModal.update act model.folderModal }
        |> fxNone
    VideoSaved (Ok video) ->
      let
        folderId = getParam "folderId" model.routerPayload
      in
        (closeModals model, loadFolderContents folderId)
    VideoSaved (Err video) ->
      fxNone model
    FolderSaved (Ok video) ->
      let
        folderId = getParam "folderId" model.routerPayload
      in
        (closeModals model, loadFolderContents folderId)
    FolderSaved (Err video) ->
      fxNone model
    CreateOrPatchFolder ->
      let
        params = FolderModal.asParams model.folderModal
        id = Maybe.withDefault 0 model.folderModal.form.id
        folderId = getParam "folderId" model.routerPayload
        params' = params ++ [("folderId", J.int folderId)]
      in
        if FolderModal.isNew model.folderModal then
          (model, createFolder params' FolderSaved)
        else
          (model, patchFolder id params' FolderSaved)
    CreateOrPatchVideo ->
      let
        params = VideoModal.asParams model.videoModal
        id = Maybe.withDefault 0 model.videoModal.form.id
        folderId = getParam "folderId" model.routerPayload
        params' = params ++ [("folderId", J.int folderId)]
      in
        if VideoModal.isNew model.videoModal then
          (model, createVideo params' VideoSaved)
        else
          (model, patchVideo id params' VideoSaved)
    OpenFolderModal Nothing ->
      { model | folderModal = FolderModal.open model.folderModal }
        |> closeDropdowns False
        |> fxNone
    OpenFolderModal (Just folderId) ->
      let
        folder = List.Extra.find (\item -> item.id == folderId) model.folds
      in
        case folder of
          Just fold ->
            { model | folderModal = FolderModal.openWithFolder fold model.folderModal }
              |> closeDropdowns False
              |> fxNone
          _ -> fxNone model
    OpenVideoModal Nothing ->
      { model | videoModal = VideoModal.open model.videoModal }
        |> closeDropdowns False
        |> fxNone
    OpenVideoModal (Just videoId) ->
      let
        video = List.Extra.find (\item -> item.id == videoId) model.vids
      in
        case video of
          Just vid ->
            { model | videoModal = VideoModal.openWithVideo vid model.videoModal }
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
    FolderContentsLoaded (Ok contents) ->
      { model | folders = List.map (\item -> FolderComponent.init item "folder") contents.folders
              , videos = List.map (\item -> FolderComponent.init item "video") contents.videos
              , vids = contents.videos
              , folds = contents.folders }
        |> fxNone
    MouseIsDown pressed ->
      closeDropdowns pressed model
        |> fxNone

    FolderAction id act ->
      let
        updatedFolders view =
          if view.id == id then
            FolderComponent.update act view
          else
            view
      in
        { model | folders = List.map updatedFolders model.folders }
        |> fxNone

    VideoAction id act ->
      let
        updatedVideos view =
          if view.id == id then
            FolderComponent.update act view
          else
            view
      in
        { model | videos = List.map updatedVideos model.videos }
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

closeDropdowns pressed model =
  let
    updatedItems view =
      FolderComponent.handleClick pressed view
  in
    { model | folders = List.map updatedItems model.folders
            , videos = List.map updatedItems model.videos
            , fabMenu = Ui.DropdownMenu.handleClick pressed model.fabMenu
            }

closeModals model =
  { model | videoModal = VideoModal.close model.videoModal
          , folderModal = FolderModal.close model.folderModal }

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
