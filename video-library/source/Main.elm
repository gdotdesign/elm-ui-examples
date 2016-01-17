module Main where

import Ext.Signal2 exposing ((>>>))
import Json.Encode as J
import List.Extra
import StartApp
import Keyboard
import Effects
import String
import Mouse
import Dict
import Task
import Hop

import Html.Attributes exposing (src, controls)
import Html.Events exposing (onClick)
import Html exposing (node, text)

import Ui.NotificationCenter
import Ui.DropdownMenu
import Ui.SearchInput
import Ui.Container
import Ui.App
import Ui

import Components.FolderView as FolderView
import Components.Modal as Modal
import Components.Item as Item
import Types exposing (..)

import Forms.Folder as FolderForm
import Forms.Video as VideoForm

import Debug exposing (log)

-- Main entry point
type alias Model =
  { folderModal : Modal.Model FolderForm.Model Folder FolderForm.Action
  , videoModal: Modal.Model VideoForm.Model Video VideoForm.Action
  , notifications : Ui.NotificationCenter.Model
  , fabMenu: Ui.DropdownMenu.Model
  , folder : Maybe FolderContents
  , search : Ui.SearchInput.Model
  , folderView : FolderView.Model
  , routerPayload : Hop.Payload
  , folders : List Folder
  , videos : List Video
  , video: Maybe Video
  , app: Ui.App.Model
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
  | Search Ui.SearchInput.Action
  | FolderView FolderView.Action
  | App Ui.App.Action
  -- Lifecycle
  | OpenFolderModal (Maybe Int)
  | OpenVideoModal (Maybe Int)
  | CreateOrPatchFolder
  | CreateOrPatchVideo
  | MouseIsDown Bool
  | DeleteFolder Int
  | DeleteVideo Int
  | EscIsDown Bool
  -- Modals
  | FolderModal (Modal.Action FolderForm.Action)
  | VideoModal (Modal.Action VideoForm.Action)
  | SS String

init : Model
init =
  let
    fabMenu = Ui.DropdownMenu.init
  in
    { notifications = Ui.NotificationCenter.init 4000 320
    , folderView = FolderView.init [] []
    , app = Ui.App.init "Video Library"
    , routerPayload = router.payload
    , search = Ui.SearchInput.init 1000
    , folder = Nothing
    , video = Nothing
    , folders = []
    , videos = []
    , fabMenu = { fabMenu | offsetY = 10
                          , favoredSides = { horizontal = "left"
                                           , vertical = "top"
                                           }
                }
    , folderModal = Modal.init { fromEntity = FolderForm.fromFolder
                               , asParams = FolderForm.asParams
                               , isValid = FolderForm.isValid
                               , update = FolderForm.update
                               , isNew = FolderForm.isNew
                               , init = FolderForm.init
                               }
    , videoModal = Modal.init { fromEntity = VideoForm.fromVideo
                              , asParams = VideoForm.asParams
                              , isValid = VideoForm.isValid
                              , update = VideoForm.update
                              , isNew = VideoForm.isNew
                              , init = VideoForm.init
                              }
    }

updateWithEffects : Action -> Model -> (Model, Effects.Effects Action)
updateWithEffects action model =
  case action of
    -- Navigation
    SS value ->
      let
        a = log "a" value
      in
        (model, Effects.none)
    HandleChange payload ->
      let
        -- Get updated ids
        videoId = getParam "videoId" payload
        folderId = getParam "folderId" payload

        -- Get current ids
        currentVideoId = getParam "videoId" model.routerPayload
        currentFolderId = getParam "folderId" model.routerPayload

        -- Get effects to load a new folder if neccessary
        folderEffects =
          if folderId /= currentFolderId then
            loadFolderContents folderId
          else
            Effects.none

        -- Get effects to load a video if neccessary
        videoEffects =
          if currentVideoId == 0 && videoId /= 0 then
            fetchVideo videoId VideoLoaded
          else
            Effects.none

        -- Update the model
        updatedModel =
          { model | routerPayload = payload
                  , video = if videoId == 0 then Nothing else model.video }
      in
        (updatedModel, Effects.batch [folderEffects, videoEffects])
    NavigateTo query ->
      (model, Effects.map HopAction (Hop.addQuery model.routerPayload.url query))

    -- Components
    Search act ->
      let
        (search, effect) =
          Ui.SearchInput.update act model.search
      in
        ({ model | search = search }, Effects.map Search effect)
    Notifications act ->
      let
        (notifications, effect) =
          Ui.NotificationCenter.update act model.notifications
      in
        ({ model | notifications = notifications }, Effects.map Notifications effect)

    -- Failed Requests
    FolderContentsLoaded (Err message) -> notify message model
    FolderSaved (Err message) -> notify message model
    VideoLoaded (Err message) -> notify message model
    VideoSaved (Err message) -> notify message model
    Refresh (Err message) -> notify message model

    -- Requests
    FolderSaved (Ok video) -> refresh model
    VideoSaved (Ok video) -> refresh model
    Refresh (Ok _) -> refresh model

    DeleteFolder id ->
      (closeDropdowns False model, deleteFolder id Refresh)
    DeleteVideo id ->
      (closeDropdowns False model, deleteVideo id Refresh)

    CreateOrPatchFolder ->
      let
        (id, params) =
          getParams
            (Modal.asParams model.folderModal)
            model.folderModal.form.id
            model
      in
        if Modal.isNew model.folderModal then
          (model, createFolder params FolderSaved)
        else
          (model, patchFolder id params FolderSaved)

    CreateOrPatchVideo ->
      let
        (id, params) =
          getParams
            (Modal.asParams model.videoModal)
            model.videoModal.form.id
            model
      in
        if Modal.isNew model.videoModal then
          (model, createVideo params VideoSaved)
        else
          (model, patchVideo id params VideoSaved)

    FolderModal act ->
      let
        (folderModal, effect) = Modal.update act model.folderModal
      in
        ({ model | folderModal = folderModal }, Effects.map FolderModal effect)
    VideoModal act ->
      let
        (videoModal, effect) = Modal.update act model.videoModal
      in
        ({ model | videoModal = videoModal }, Effects.map VideoModal effect)

    App act ->
      let
        (app, effect) = Ui.App.update act model.app
      in
        ({ model | app = app }, Effects.map App effect)

    -- Handle Esc
    EscIsDown isDown ->
      let
        query = createQuery Nothing (Just 0) model.routerPayload
      in
        if isDown then
          (model, Effects.task (Task.succeed (NavigateTo query)))
        else
          closeModals model
          |> closeDropdowns isDown
          |> fxNone
    _ ->
      (update action model, Effects.none)

update: Action -> Model -> Model
update action model =
  case action of
    -- Components
    FolderView act ->
      { model | folderView = FolderView.update act model.folderView }
    FabMenu act ->
      { model | fabMenu = Ui.DropdownMenu.update act model.fabMenu }

    -- Folder Modal
    OpenFolderModal Nothing ->
      { model | folderModal = Modal.open model.folderModal }
        |> closeDropdowns False
    OpenFolderModal (Just folderId) ->
      let
        folder = List.Extra.find (\item -> item.id == folderId) model.folders
      in
        Maybe.map (openFolderModal model) folder
        |> Maybe.withDefault model

    -- Video Modal
    OpenVideoModal Nothing ->
      { model | videoModal = Modal.open model.videoModal }
        |> closeDropdowns False
    OpenVideoModal (Just videoId) ->
      let
        video = List.Extra.find (\item -> item.id == videoId) model.videos
      in
        Maybe.map (openVideoModal model) video
        |> Maybe.withDefault model

    -- Responses
    VideoLoaded (Ok video) ->
      { model | video = Just video }
    FolderContentsLoaded (Ok contents) ->
      { model | folderView = FolderView.init contents.folders contents.videos
              , folder = Just contents
              , videos = contents.videos
              , folders = contents.folders }

    -- MouseDown
    MouseIsDown pressed ->
      closeDropdowns pressed model

    _ -> model

view: Signal.Address Action -> Model -> Html.Html
view address model =
  let
    breadcrumbItems =

      let
        base = [("Library", NavigateTo (createQuery (Just 0) Nothing model.routerPayload))]
        last =
          case model.folder of
            Just folder ->
              if folder.id == 0 then
                []
              else
                [(folder.name, NavigateTo (createQuery (Just folder.id) Nothing model.routerPayload))]
            _ -> []
        parts =
          case model.folder of
            Just folder ->
              List.filter (\fold -> fold.id /= 0) folder.breadcrumbs
              |> List.map (\parent-> (parent.name, NavigateTo (createQuery (Just parent.id) Nothing model.routerPayload)))
              |> List.reverse
            _ -> []
      in
        base ++ parts ++ last
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
    Ui.App.view (address >>> App) model.app
      [ Ui.NotificationCenter.view
          (address >>> Notifications)
          model.notifications
      , Modal.view
          (address >>> VideoModal)
          { saveTexts = ("Edit Video", "Save")
          , newTexts = ("Add Video", "Add")
          , action = CreateOrPatchVideo
          , view = VideoForm.view
          , address = address
          }
          model.videoModal
      , Modal.view
          (address >>> FolderModal)
          { saveTexts = ("Edit Folder", "Save")
          , newTexts = ("Add Folder", "Add")
          , action = CreateOrPatchFolder
          , view = FolderForm.view
          , address = address
          }
          model.folderModal
      , node "video-library" []
        [ Ui.Container.view { align = "stretch"
                            , direction = "column"
                            , compact = True } []
              [ Ui.header []
                [ Ui.headerTitle [] [text "My Video Library"]
                , Ui.SearchInput.view (address >>> Search) model.search
                ]
              , breadcrumbs address (node "span" [] [text "/"]) breadcrumbItems
              , FolderView.view
                (address >>> FolderView)
                { videoActions = videoActions address model
                , folderActions = folderActions address model
                }
                model.folderView
              ]
        ]
      , videoPlayer
      , Ui.DropdownMenu.view
        (address >>> FabMenu)
        (Ui.fab "plus" [])
        [ Item.menuItem "android-film" "Add Video"
            (onClick address (OpenVideoModal Nothing))
        , Item.menuItem "folder" "Add Folder"
            (onClick address (OpenFolderModal Nothing))
        ]
        model.fabMenu
      ]

-- View functions
videoActions : Signal.Address Action -> Model -> Item.Model -> Item.ViewModel
videoActions address base model =
  let
    query = createQuery Nothing (Just model.id) base.routerPayload
  in
    { onEdit   = onClick address (OpenVideoModal (Just model.id))
    , onDelete = onClick address (DeleteVideo model.id)
    , onClick  = onClick address (NavigateTo query)
    }

folderActions : Signal.Address Action -> Model -> Item.Model -> Item.ViewModel
folderActions address base model =
  let
    query = createQuery (Just model.id) Nothing base.routerPayload
  in
    { onEdit   = onClick address (OpenFolderModal (Just model.id))
    , onDelete = onClick address (DeleteFolder model.id)
    , onClick  = onClick address (NavigateTo query)
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

-- Routing functions
routes : List (String, Hop.Payload -> Action)
routes =
  [ ("/", HandleChange) ]

router : Hop.Router Action
router =
  Hop.new
    { notFoundAction = HandleChange
    , routes = routes
    }

createQuery : Maybe Int -> Maybe Int -> Hop.Payload -> Dict.Dict String String
createQuery folderId videoId payload =
  let
    param name id =
      toString (Maybe.withDefault (getParam name payload) id)
  in
    Dict.fromList
      [ ("folderId", param "folderId" folderId)
      , ("videoId", param "videoId" videoId)
      ]

getParam : String -> Hop.Payload -> Int
getParam key payload =
  payload.params
    |> Dict.get key
    |> Maybe.withDefault ""
    |> String.toInt
    |> Result.withDefault 0

-- Query Functions
loadFolderContents : Int -> Effects.Effects Action
loadFolderContents id =
  fetchFolderContents id FolderContentsLoaded

refresh : Model -> (Model, Effects.Effects Action)
refresh model =
  let
    folderId = getParam "folderId" model.routerPayload
  in
    (closeModals model, loadFolderContents folderId)

getParams : List (String, J.Value) -> Maybe Int -> Model
          -> (Int, List (String, J.Value))
getParams baseParams maybeId model =
  let
    id = Maybe.withDefault 0 maybeId
    folderId = getParam "folderId" model.routerPayload
  in
    (id, baseParams ++ [("folder_id", J.int folderId)])

-- Lifecycle functions
openVideoModal : Model -> Video -> Model
openVideoModal model video =
  { model | videoModal = Modal.openWithEntity video model.videoModal }
    |> closeDropdowns False

openFolderModal : Model -> Folder -> Model
openFolderModal model folder =
  { model | folderModal = Modal.openWithEntity folder model.folderModal }
    |> closeDropdowns False

notify : String -> Model -> (Model, Effects.Effects Action)
notify message model =
  let
    (notifications, effect) =
      Ui.NotificationCenter.notify (text message) model.notifications
  in
    ({ model | notifications = notifications }, Effects.map Notifications effect)

closeDropdowns : Bool -> Model -> Model
closeDropdowns pressed model =
  { model | folderView = FolderView.handleClick pressed model.folderView
          , fabMenu = Ui.DropdownMenu.handleClick pressed model.fabMenu
          }

closeModals : Model -> Model
closeModals model =
  { model | videoModal = Modal.close model.videoModal
          , folderModal = Modal.close model.folderModal }

fxNone : Model -> (Model, Effects.Effects action)
fxNone model =
  (model, Effects.none)

-- Start App
app =
  let
    initial = init
  in
    StartApp.start { init = (init, loadFolderContents 0)
                   , view = view
                   , update = updateWithEffects
                   , inputs = [ Signal.dropRepeats router.signal
                              , Signal.map EscIsDown (Keyboard.isDown 27)
                              , Signal.map MouseIsDown Mouse.isDown
                              , Signal.map SS init.search.mailbox.signal] }

main =
  app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
