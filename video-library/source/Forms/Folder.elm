module Forms.Folder exposing (..)

import Html exposing (node, text)

import Json.Encode as J
import String

import Ui.Container
import Ui.Input
import Ui

import Types exposing (Folder)

type alias Model =
  { image : Ui.Input.Model
  , name : Ui.Input.Model
  , id : Maybe Int
  }

type Msg
  = Image Ui.Input.Msg
  | Name Ui.Input.Msg

init : Model
init =
  { image = Ui.Input.init "" "Image URL..."
  , name = Ui.Input.init "" "Name..."
  , id = Nothing
  }

fromFolder : Folder -> Model
fromFolder {image, name, id} =
  { image = Ui.Input.init image "Image URL..."
  , name = Ui.Input.init name "Name..."
  , id = Just id
  }

asParams : Model -> List (String, J.Value)
asParams model =
  [ ("image", J.string model.image.value)
  , ("name", J.string model.name.value)
  ]

isNew : Model -> Bool
isNew model =
  model.id == Nothing

isValid : Model -> Bool
isValid model =
  not (String.isEmpty (String.trim model.image.value)) &&
  not (String.isEmpty (String.trim model.name.value))

view: Model -> Html.Html Msg
view model =
  node "video-library-folder-form" []
    [ Ui.Container.column []
      [ Ui.inputGroup "Image" (Html.map Image (Ui.Input.view model.image))
      , Ui.inputGroup "Name" (Html.map Name (Ui.Input.view model.name))
      ]
    ]

update: Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Image act ->
      let
        (image, cmd) = Ui.Input.update act model.image
      in
        ({ model | image = image }, Cmd.map Image cmd)
    Name act ->
      let
        (name, cmd) = Ui.Input.update act model.name
      in
        ({ model | name = name }, Cmd.map Name cmd)
