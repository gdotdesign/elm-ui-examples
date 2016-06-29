module Main exposing (..)

{-| This is an example showing how to load and display images in Elm-UI.
-}

import Numeral exposing (format)
import Json.Decode as Json
import Task
import Html exposing (div, node, span, strong, text, img)
import Html.Attributes exposing (src, class)
import Html.Events exposing (on)
import Html.App
import Ui.Native.FileManager as FileManager exposing (File)
import Ui.Container
import Ui.Button
import Ui.App
import Ui


{-| Representation of an image viewer:
  - **dataURI** - This holds the current image as a dataURI
  - **image** - This holds the current image as a file
  - **size** - This holds the image size
  - **app** - The base app component
-}
type alias Model =
  { dataURI : Maybe String
  , size : ( Int, Int )
  , image : Maybe File
  , app : Ui.App.Model
  }


{-| Messages that an image viewer can receive.
-}
type Msg
  = UpdateSize ( Int, Int )
  | App Ui.App.Msg
  | Opened File
  | Read String
  | Open


{-| Initializes an image viewer.
-}
init : Model
init =
  { app = Ui.App.init "Elm-UI Project"
  , dataURI = Nothing
  , image = Nothing
  , size = ( 0, 0 )
  }


{-| Updates an image viewer.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    -- App Updates
    App act ->
      let
        ( app, effect ) =
          Ui.App.update act model.app
      in
        ( { model | app = app }, Cmd.map App effect )

    -- Save the dataURI after reading it
    Read dataURI ->
      ( { model | dataURI = Just dataURI }, Cmd.none )

    -- Save the image size
    UpdateSize size ->
      ( { model | size = size }, Cmd.none )

    -- Save the image after opening it and start reading it
    Opened image ->
      let
        cmd =
          Task.perform (\_ -> Debug.crash "")
            Read
            (FileManager.readAsDataURL image)
      in
        ( { model | image = Just image }, cmd )

    -- Open a file browser to load an image
    Open ->
      let
        cmd =
          Task.perform (\_ -> Debug.crash "")
            Opened
            (FileManager.openSingle "image/*")
      in
        ( model, cmd )


{-| Renders an image viewer
-}
view : Model -> Html.Html Msg
view model =
  let
    hasImage =
      Maybe.map (\_ -> "has-image") model.image
        |> Maybe.withDefault ""
  in
    Ui.App.view App
      model.app
      [ node "image-preview"
          [ class hasImage ]
          [ node "image-preview-header"
              []
              [ renderHeader
              , Ui.Button.primaryBig "Load Image" Open
              ]
          , node "image-preview-content" [] [ renderImage model ]
          , node "image-preview-info" [] (renderInfo model)
          ]
      ]


{-| Decodes an images size.
-}
decodeImageSize : Json.Decoder ( Int, Int )
decodeImageSize =
  Json.object2 (,)
    (Json.at [ "target", "naturalWidth" ] Json.int)
    (Json.at [ "target", "naturalHeight" ] Json.int)


{-| Renders the header.
-}
renderHeader : Html.Html Msg
renderHeader =
  div []
    [ node "h1" [] [ text "Image Preview" ]
    , node "p"
        []
        [ text "This is an example on how to load and read files in Elm-UI" ]
    ]


{-| Renders file information.
-}
renderInfo : Model -> List (Html.Html Msg)
renderInfo model =
  case model.image of
    Just image ->
      let
        dimensions =
          (toString (fst model.size))
            ++ "px x "
            ++ (toString (snd model.size)
                  ++ "px"
               )
      in
        [ text
            ("File name: "
              ++ image.name
              ++ "  /  Size: "
              ++ (format "0.0b" image.size)
              ++ "  /  Dimensions: "
              ++ dimensions
            )
        ]

    Nothing ->
      []


{-| Renders the image.
-}
renderImage : Model -> Html.Html Msg
renderImage model =
  case model.dataURI of
    Just dataURI ->
      img [ src dataURI, on "load" (Json.map UpdateSize decodeImageSize) ] []

    Nothing ->
      node "span" [] [ text "No image is loaded!" ]


{-| Main entry point.
-}
main =
  Html.App.program
    { init = ( init, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
