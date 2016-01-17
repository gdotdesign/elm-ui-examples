module Components.Item where

import Ext.Signal2 exposing ((>>>))

import Html.Attributes exposing (classList, style)
import Html exposing (node, text)

import Ui.DropdownMenu as DropdownMenu
import Ui.Container
import Ui

import Types exposing (..)

type alias Model =
  { menu : DropdownMenu.Model
  , image : String
  , name : String
  , kind : String
  , id : Int
  }

type alias Entity a =
  { a | id : Int
      , name : String
      , image : String
  }

type alias ViewModel =
  { onDelete : Html.Attribute
  , onClick : Html.Attribute
  , onEdit : Html.Attribute
  }

type Action
  = Menu DropdownMenu.Action

update: Action -> Model -> Model
update action model =
  case action of
    Menu act ->
      { model | menu = DropdownMenu.update act model.menu }

init : String -> Entity a -> Model
init kind {id, name, image} =
  let
    menu =
      DropdownMenu.init

    favoredSides =
      { horizontal = "right"
      , vertical = "bottom"
      }
  in
    { menu = { menu | favoredSides = favoredSides }
    , image = image
    , kind = kind
    , name = name
    , id = id
    }

-- TODO: Move this to Elm-UI DropdownMenu
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
    [ classList [(model.kind, True)] ]
    [ node "video-library-item-image"
      [ viewModel.onClick
      , style [("background-image", "url(" ++ model.image ++ ")") ]
      ]
      [ ]
    , Ui.Container.row []
        [ node "div" [viewModel.onClick] [text model.name]
        , DropdownMenu.view
          (address >>> Menu)
          (Ui.icon "android-more-horizontal" True [])
          [ menuItem "android-open" "Open" viewModel.onClick
          , menuItem "edit" "Edit" viewModel.onEdit
          , menuItem "trash-b" "Delete" viewModel.onDelete
          ]
          model.menu
        ]
    ]
