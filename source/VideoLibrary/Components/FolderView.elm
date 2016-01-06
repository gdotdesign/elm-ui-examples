module VideoLibrary.Components.FolderView where

import Ext.Signal exposing ((>>>))

import Html exposing (node)

import VideoLibrary.Components.Item as Item
import VideoLibrary.Types exposing (..)

type alias Model =
  { folders: List Item.Model
  , videos : List Item.Model
  }

type alias ViewModel =
  { videoActions : Item.Model -> Item.ViewModel
  , folderActions : Item.Model -> Item.ViewModel
  }

type Action
  = FolderAction Int Item.Action
  | VideoAction Int Item.Action


init : List Folder -> List Video -> Model
init folders videos =
  { videos = List.map (Item.init "video") videos
  , folders = List.map (Item.init "folder") folders
              |> List.filter (\item -> item.id /= 0)

  }

update: Action -> Model -> Model
update action model =
  case action of
    FolderAction id act ->
      let
        updatedFolders view =
          if view.id == id then
            Item.update act view
          else
            view
      in
        { model | folders = List.map updatedFolders model.folders }
    VideoAction id act ->
      let
        updatedVideos view =
          if view.id == id then
            Item.update act view
          else
            view
      in
        { model | videos = List.map updatedVideos model.videos }

view: Signal.Address Action -> ViewModel -> Model -> Html.Html
view address viewModel model =
  node "video-library-folder" []
    ((List.map (renderItem address (viewModel.folderActions) FolderAction) model.folders)
      ++
      (List.map (renderItem address (viewModel.videoActions) VideoAction) model.videos)
    )

handleClick pressed model =
  { model | folders = List.map (Item.handleClick pressed) model.folders
          , videos = List.map (Item.handleClick pressed) model.videos
  }

renderItem : Signal.Address Action -> (Item.Model -> Item.ViewModel)
           -> (Int -> Item.Action -> Action) -> Item.Model -> Html.Html
renderItem address viewModel action model =
  Item.view
    (address >>> (action model.id))
    (viewModel model)
    model
