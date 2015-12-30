module VideoLibrary.Components.Folder where

import Signal exposing (forwardTo)

import Html.Attributes exposing (classList, style)
import Html exposing (node, text)

import Ui.DropdownMenu as DropdownMenu
import Ui.Container
import Ui

import VideoLibrary.Types exposing (..)

type alias Model =
  { menu : DropdownMenu.Model
  , image : String
  , name : String
  , id : String
  }

type alias ViewModel =
  { onClick : Html.Attribute
  , onDelete : Html.Attribute
  , onOpen : Html.Attribute
  }

type Action
  = Menu DropdownMenu.Action

update: Action -> Model -> Model
update action model =
  case action of
    Menu act ->
      { model | menu = DropdownMenu.update act model.menu }

init : { a | id : String, name : String, image : String } -> Model
init {id, name, image}  =
  let
    menu = DropdownMenu.init
  in
    { menu = { menu | favoredSides = { horizontal = "right"
                                     , vertical = "bottom"
                                     }
             }
    , image = image
    , name = name
    , id = id
    }

menuItem : String -> String -> Html.Attribute -> Html.Html
menuItem icon title action =
  DropdownMenu.item [action]
    [ Ui.icon icon False []
    , node "span" [] [text title ]
    ]

handleClick : Bool -> Model -> Model
handleClick pressed model =
  { model | menu = DropdownMenu.handleClick pressed model.menu }

closeMenu : Model -> Model
closeMenu model =
  { model | menu = DropdownMenu.close model.menu }

view : Signal.Address Action -> ViewModel -> Model -> Html.Html
view address viewModel model =
  node "video-library-item"
    [ ]
    [ node "video-library-item-image"
      [ viewModel.onClick
      , style [("background-image", "url(" ++ model.image ++ ")") ]
      ]
      [ ]
    , Ui.Container.row []
        [ node "div" [] [text model.name]
        , DropdownMenu.view
          (forwardTo address Menu)
          (Ui.icon "android-more-horizontal" True [])
          [ menuItem "trash-b" "Delete" viewModel.onDelete
          , menuItem "android-open" "Open" viewModel.onOpen
          ]
          model.menu
        ]
    ]
