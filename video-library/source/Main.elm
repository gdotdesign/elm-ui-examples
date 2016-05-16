module Main exposing (..)

import Html exposing (node, text)
import Html.App

import Ui.Helpers.Emitter as Emitter
import Ui.NotificationCenter
import Ui.Container
import Ui.Layout
import Ui.Header
import Ui.App
import Ui

import Components.Folder as Folder
import Components.Player as Player
import Components.Modal as Modal
import Components.Item as Item

import Forms.Folder as FolderForm
import Forms.Video as VideoForm

import Types exposing (..)

import Debug exposing (log)

type alias Model =
  { folderModal : Modal.Model FolderForm.Model Folder FolderForm.Msg
  , videoModal: Modal.Model VideoForm.Model Video VideoForm.Msg
  , notifications : Ui.NotificationCenter.Model Msg
  , player : Player.Model
  , folder : Folder.Model
  , app: Ui.App.Model
  }


type Msg
  = NoOp
  | Notifications Ui.NotificationCenter.Msg
  | App Ui.App.Msg
  | Folder Folder.Msg
  | FolderModal (Modal.Action Folder FolderForm.Msg)
  | VideoModal (Modal.Action Video VideoForm.Msg)
  | Player Player.Msg
  | Notify String

init : Model
init =
  { notifications = Ui.NotificationCenter.init 4000 320
  , folder = Folder.init
  , app = Ui.App.init "Video Library"
  , player = Player.init
  , folderModal = Modal.init { fromEntity = FolderForm.fromFolder
                             , asParams = FolderForm.asParams
                             , isValid = FolderForm.isValid
                             , update = FolderForm.update
                             , isNew = FolderForm.isNew
                             , init = FolderForm.init
                             , view = FolderForm.view
                             , newTexts = ("Add New Folder", "Add")
                             , editTexts = ("Edit Folder", "Save")
                             , id = .id
                             , get = Types.fetchFolder
                             , patch = Types.patchFolder
                             , create = Types.createFolder
                             }
  , videoModal = Modal.init { fromEntity = VideoForm.fromVideo
                            , asParams = VideoForm.asParams
                            , isValid = VideoForm.isValid
                            , update = VideoForm.update
                            , isNew = VideoForm.isNew
                            , init = VideoForm.init
                            , view = VideoForm.view
                            , newTexts = ("Add New Video", "Add")
                            , editTexts = ("Edit Video", "Save")
                            , id = .id
                            , get = Types.fetchVideo
                            , patch = Types.patchVideo
                            , create = Types.createVideo
                            }
  }

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case log "a" msg of
    Notify message ->
      let
        (notifications, effect) =
          Ui.NotificationCenter.notify (text message) model.notifications
      in
        ({ model | notifications = notifications }, Cmd.map Notifications effect)

    Folder act ->
      let
        (folder, cmd) = Folder.update act model.folder
      in
        ({ model | folder = folder }, Cmd.map Folder cmd)

    Player act ->
      let
        (player, cmd) = Player.update act model.player
      in
        ({ model | player = player }, Cmd.map Player cmd)

    FolderModal act ->
      let
        (folderModal, cmd) = Modal.update act model.folderModal
      in
        ({ model | folderModal = folderModal }, Cmd.map FolderModal cmd)

    VideoModal act ->
      let
        (videoModal, cmd) = Modal.update act model.videoModal
      in
        ({ model | videoModal = videoModal }, Cmd.map VideoModal cmd)

    Notifications act ->
      let
        (notifications, cmd) = Ui.NotificationCenter.update act model.notifications
      in
        ({ model | notifications = notifications }, Cmd.map Notifications cmd)

    App act ->
      let
        (app, cmd) = Ui.App.update act model.app
      in
        ({ model | app = app }, Cmd.map App cmd)

    _ -> (model, Cmd.none)

view: Model -> Html.Html Msg
view model =
  Ui.App.view App model.app
    [ Ui.NotificationCenter.view Notifications model.notifications
    , Html.App.map FolderModal (Modal.view model.folderModal)
    , Html.App.map VideoModal (Modal.view model.videoModal)
    , Html.App.map Player (Player.view model.player)
    , node "video-library" []
      [ Ui.Header.view []
        [ Ui.Header.title [] [text "Video Library"]
        ]
      , Html.App.map Folder (Folder.view model.folder)
      ]
    ]

main =
  let
    model = init
  in
    Html.App.program
      { init = (model, Cmd.map Folder (Folder.initialize model.folder))
      , view = view
      , update = update
      , subscriptions = \model -> Sub.batch [ Sub.map Folder (Folder.subscriptions model.folder)
                                            , Sub.map Player Player.subscriptions
                                            , Sub.map VideoModal (Modal.subscriptions "video")
                                            , Sub.map FolderModal (Modal.subscriptions "folder")
                                            , Emitter.listenString "errors" Notify ]
      }
