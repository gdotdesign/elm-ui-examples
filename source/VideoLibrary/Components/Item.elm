module VideoLibrary.Components.Item where

import Signal
import Html

import Ui.DropdownMenu as DropdownMenu

import VideoLibrary.Types exposing (..)

type alias Model =
  { menu : DropdownMenu.Model
  , item : Item
  }

type alias VideModel =
  { onClick : Html.Attribute
  }

init : Item -> Model
init item =
  { menu = DropdownMenu.init
  , item = item
  }

backgroundUrl : Item -> String
backgroundUrl item =
  "url(\"" ++ (itemImage item) ++ "\")"

view : Signal.Address Action -> ViewModel -> Model -> Html.Html
view address viewModel {item, menu} =
  let
    kind =
      case item of
        VideoNode _ -> "video"
        FolderNode _ -> "folder"
  in
    node "video-library-item"
      [ viewModel.onClick
      , classList [(kind, True)]
      ]
      [ node "video-library-item-image"
        [ style [("background-image", backgroundUrl item )]]
        []
      , Ui.Container.row []
          [ node "div" [] [text (itemName item)]
          , Ui.DropdownMenu.view model.menu [text "ASd"]
      ]
