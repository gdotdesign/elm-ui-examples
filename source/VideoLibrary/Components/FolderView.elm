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
  , folders =
      List.map (Item.init "folder") folders
      |> List.filter (\item -> item.id /= 0)
  }

updatedItem : Int -> Item.Action -> Item.Model -> Item.Model
updatedItem id act item =
  if item.id == id then
    Item.update act item
  else
    item

update: Action -> Model -> Model
update action model =
  case action of
    FolderAction id act ->
      { model | folders = List.map (updatedItem id act) model.folders }
    VideoAction id act ->
      { model | videos = List.map (updatedItem id act) model.videos }

view : Signal.Address Action -> ViewModel -> Model -> Html.Html
view address viewModel model =
  let
    folders =
      renderItems address viewModel.folderActions FolderAction model.folders
    videos =
      renderItems address viewModel.videoActions VideoAction model.videos
  in
    node "video-library-folder" [] (folders ++ videos)

renderItems : Signal.Address Action -> (Item.Model -> Item.ViewModel)
            -> (Int -> Item.Action -> Action) -> List Item.Model -> List Html.Html
renderItems address actions action items =
  List.map (renderItem address actions action) items

handleClick : Bool -> Model -> Model
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
