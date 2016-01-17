module Components.FolderView where

import Html exposing (node, text, div, b, span)
import Ext.Signal2 exposing ((>>>))
import Effects

import Components.Item as Item
import Types exposing (..)

import Ui.Loader
import Ui

type alias Model =
  { folders: List Item.Model
  , videos : List Item.Model
  , loader : Ui.Loader.Model
  }

type alias ViewModel =
  { videoActions : Item.Model -> Item.ViewModel
  , folderActions : Item.Model -> Item.ViewModel
  }

type Action
  = FolderAction Int Item.Action
  | VideoAction Int Item.Action
  | Loader Ui.Loader.Action

init : Model
init =
  { folders = []
  , videos = []
  , loader = Ui.Loader.init 200 "normal"
  }

setData : List Folder -> List Video -> Model -> Model
setData folders videos model =
  { model | videos = List.map (Item.init "video") videos
          , folders =
              List.map (Item.init "folder") folders
              |> List.filter (\item -> item.id /= 0)
  }

updateLoading : Bool -> Model -> (Model, Effects.Effects Action)
updateLoading loading model =
  let
    (loader, effect) = Ui.Loader.start model.loader
    (loader2) = Ui.Loader.finish model.loader
  in
    if loading then
      ({ model | loader = loader }, Effects.map Loader effect)
    else
      ({ model | loader = loader2}, Effects.none)

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
    Loader act ->
      { model | loader = Ui.Loader.update act model.loader }
emptyView : Html.Html
emptyView =
  node "video-library-folder-empty" []
    [ div []
      [ div []
        [ text "There are no "
        , b [] [text "folders"]
        ]
      , div []
        [ text "or "
        , b [] [text "videos"]
        , text " here yet!"
        ]
      ]
    , Ui.icon "ios-film" False []
    , div []
      [ div [] [text "Add one by clicking on the "
               , Ui.icon "plus-circled" False []
               , span [] [text " icon"]
               ]
      , div [] [text "at the bottom right corner."]
      ]
    ]

view : Signal.Address Action -> ViewModel -> Model -> Html.Html
view address viewModel model =
  let
    folders =
      renderItems address viewModel.folderActions FolderAction model.folders

    videos =
      renderItems address viewModel.videoActions VideoAction model.videos

    contents =
      if List.isEmpty (model.folders ++ model.videos) then
        [emptyView]
      else
        (folders ++ videos)
  in
    node "video-library-folder" [] (contents ++ [Ui.Loader.view model.loader])

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
