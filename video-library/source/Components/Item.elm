module Components.Item exposing (..)

import Html.Attributes exposing (classList, style)
import Html.Events exposing (onClick)
import Html exposing (node, text)

import Json.Decode as Json

import Ui.Helpers.Emitter as Emitter
import Ui.DropdownMenu
import Ui.Container
import Ui

import Types

type alias Model =
  { menu : Ui.DropdownMenu.Model
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

type Msg
  = Menu Ui.DropdownMenu.Msg
  | Open
  | Delete
  | Edit
  | Deleted Json.Value
  | Error String

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map Menu (Ui.DropdownMenu.subscriptions model.menu)

update: Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Open ->
      (closeMenu model, Emitter.sendInt ("open-" ++ model.kind) model.id)

    Edit ->
      (closeMenu model, Emitter.sendInt ("edit-" ++ model.kind) model.id)

    Deleted _ ->
      (closeMenu model, Emitter.sendNaked "refresh")

    Error message ->
      (model, Emitter.sendString "errors" message)

    Delete ->
      let
        cmd =
          if model.kind == "video" then
            Types.deleteVideo model.id Error Deleted
          else
            Types.deleteFolder model.id Error Deleted
      in
        (model, cmd)

    Menu act ->
      ({ model | menu = Ui.DropdownMenu.update act model.menu }, Cmd.none)

init : String -> Entity a -> Model
init kind {id, name, image} =
  let
    menu =
      Ui.DropdownMenu.init

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
menuItem : String -> String -> msg -> Html.Html msg
menuItem icon title action =
  Ui.DropdownMenu.item [onClick action]
    [ Ui.icon icon False []
    , node "span" [] [text title ]
    ]

closeMenu : Model -> Model
closeMenu model =
  { model | menu = Ui.DropdownMenu.close model.menu }

view : Model -> Html.Html Msg
view model =
  node "video-library-item"
    [ classList [(model.kind, True)]
    ]
    [ node "video-library-item-image"
      [ onClick Open
      , style [("background-image", "url(" ++ model.image ++ ")") ]
      ]
      [ ]
    , Ui.Container.row []
        [ node "div" [onClick Open] [text model.name]
        , Ui.DropdownMenu.view
          { element = (Ui.icon "android-more-horizontal" True [])
          , items = [ menuItem "android-open" "Open" Open
                    , menuItem "edit" "Edit" Edit
                    , menuItem "trash-b" "Delete" Delete
                    ]
          }
          Menu
          model.menu
        ]
    ]
