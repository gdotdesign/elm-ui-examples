module Forms.Folder where

import Ext.Signal2 exposing ((>>>))
import Html exposing (node, text)
import Json.Encode as J
import Maybe.Extra
import Effects
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

type Action
  = Image Ui.Input.Action
  | Name Ui.Input.Action

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
  Maybe.Extra.isNothing model.id

isValid : Model -> Bool
isValid model =
  not (String.isEmpty (String.trim model.image.value)) &&
  not (String.isEmpty (String.trim model.name.value))

view: Signal.Address Action -> Model -> Html.Html
view address model =
  node "video-library-folder-form" []
    [ Ui.Container.column []
      [ Ui.inputGroup "Image" (Ui.Input.view (address >>> Image) model.image)
      , Ui.inputGroup "Name" (Ui.Input.view (address >>> Name) model.name)
      ]
    ]

update: Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Image act ->
      let
        (image, effect) = Ui.Input.update act model.image
      in
        ({ model | image = image }, Effects.map Image effect)
    Name act ->
      let
        (name, effect) = Ui.Input.update act model.name
      in
        ({ model | name = name }, Effects.map Name effect)
