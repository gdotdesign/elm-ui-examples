module Main where

import StartApp
import Effects
import Signal exposing (forwardTo)
import Task
import Http
import Hop
import Dict
import Keyboard
import Mouse

import Html.Attributes exposing (style, src, controls, classList)
import Html.Events exposing (onClick)
import Html exposing (node, text)

import Ui.Container
import Ui.App
import Ui

import Debug exposing (log)

import VideoLibrary.Types exposing (..)
import VideoLibrary.Components.Folder as FolderComponent

-- Main entry point
type alias Model =
  { app: Ui.App.Model
  , folders: List FolderComponent.Model
  , videos : List FolderComponent.Model
  , video: Maybe Video
  , routerPayload : Hop.Payload
  }

type Action
  -- Request results
  = FoldersLoaded (Result String (List Folder))
  | VideosLoaded (Result String (List Video))
  | VideoLoaded (Result String Video)
  -- Navigation
  | NavigateTo (Dict.Dict String String)
  | HandleChange Hop.Payload
  | HopAction Hop.Action
  -- Components
  | FolderAction String FolderComponent.Action
  | VideoAction String FolderComponent.Action
  | App Ui.App.Action
  -- Lifecycle
  | MouseIsDown Bool
  | EscIsDown Bool
  | NoOp

init : Model
init =
  { app = Ui.App.init "Video Library"
  , routerPayload = router.payload
  , video = Nothing
  , folders = []
  , videos = []
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

createQuery : Maybe String -> Maybe String -> Hop.Payload -> Dict.Dict String String
createQuery folderId videoId payload =
  Dict.fromList
    [ ("folderId", Maybe.withDefault (getParam "folderId" payload) folderId)
    , ("videoId", Maybe.withDefault (getParam "videoId" payload) videoId)
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
      , onOpen = onClick address NoOp
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
      , onOpen = onClick address NoOp
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
      [ node "video-library" []
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
      ]

getParam : String -> Hop.Payload -> String
getParam key payload =
  payload.params
    |> Dict.get key
    |> Maybe.withDefault ""

loadFolderContents id =
  Effects.batch [ (fetchVideos id VideosLoaded)
                , (fetchFolders id FoldersLoaded)
                ]

update: Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    NavigateTo query ->
      (model, Effects.map HopAction (Hop.addQuery model.routerPayload.url query))
    App act ->
      { model | app = Ui.App.update act model.app }
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
          if currentVideoId == "" && videoId /= "" then (fetchVideo videoId VideoLoaded)
          else Effects.none

        updatedModel =
          { model | routerPayload = payload
                  , video = if videoId == "" then Nothing else model.video }
      in
        (updatedModel, Effects.batch [folderEffects, videoEffects])
    VideoLoaded (Ok video) ->
      { model | video = Just video }
        |> fxNone
    VideosLoaded (Ok videos) ->
      { model | videos = List.map FolderComponent.init videos }
        |> fxNone
    FoldersLoaded (Ok folders) ->
      { model | folders = List.map FolderComponent.init folders }
        |> fxNone
    MouseIsDown pressed ->
      let
        updatedItems view =
          FolderComponent.handleClick pressed view
      in
        { model | folders = List.map updatedItems model.folders
                , videos = List.map updatedItems model.videos }
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
          createQuery Nothing (Just "") model.routerPayload
      in
        if isDown then
          (model, Effects.map HopAction (Hop.addQuery model.routerPayload.url query))
        else
          fxNone model
    _ -> fxNone model

fxNone : Model -> (Model, Effects.Effects action)
fxNone model =
  (model, Effects.none)

app =
  StartApp.start { init = (init, Effects.batch [ (fetchVideos "" VideosLoaded)
                                               , (fetchFolders "" FoldersLoaded)
                                               ])
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
