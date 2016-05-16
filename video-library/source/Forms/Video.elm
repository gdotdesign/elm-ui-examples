module Forms.Video exposing (..)

import Html exposing (node, text)
import Html.App

import Json.Encode as J
import Maybe.Extra
import String

import Ui.Container
import Ui.Input
import Ui

import Types exposing (Video)

type alias Model =
  { image : Ui.Input.Model
  , name : Ui.Input.Model
  , url : Ui.Input.Model
  , id : Maybe Int
  }

type Msg
  = Image Ui.Input.Msg
  | Name Ui.Input.Msg
  | Url Ui.Input.Msg

init : Model
init =
  { image = Ui.Input.init "" "Image URL..."
  , name = Ui.Input.init "" "Name..."
  , url = Ui.Input.init "" "Video URL..."
  , id = Nothing
  }

fromVideo : Video -> Model
fromVideo {id, image, name, url} =
  { image = Ui.Input.init image "Image URL..."
  , name = Ui.Input.init name "Name..."
  , url = Ui.Input.init url "Video URL..."
  , id = Just id
  }

asParams : Model -> List (String, J.Value)
asParams model =
  [ ("image", J.string model.image.value)
  , ("name", J.string model.name.value)
  , ("url", J.string model.url.value)
  ]

isNew : Model -> Bool
isNew model =
  Maybe.Extra.isNothing model.id

isValid : Model -> Bool
isValid model =
  (not (String.isEmpty (String.trim model.image.value))) &&
  (not (String.isEmpty (String.trim model.name.value))) &&
  (not (String.isEmpty (String.trim model.url.value)))

view: Model -> Html.Html Msg
view model =
  node "video-libaray-video-form" []
    [ Ui.Container.column []
      [ Ui.inputGroup "Image" (Html.App.map Image (Ui.Input.view model.image))
      , Ui.inputGroup "Name" (Html.App.map Name (Ui.Input.view model.name))
      , Ui.inputGroup "Url" (Html.App.map Url (Ui.Input.view model.url))
      ]
    ]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
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
    Url act ->
      let
        (url, cmd) = Ui.Input.update act model.url
      in
        ({ model | url = url }, Cmd.map Url cmd)
