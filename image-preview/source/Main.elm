{-| This is an example showing how to load and display images in Elm-UI.
-}
import Numeral exposing (format)
import Json.Decode as Json
import Task exposing (Task)

import Html exposing (div, node, span, strong, text, img)
import Html.Attributes exposing (src, class, href, rel)
import Html.Events exposing (on)

import Ui.Native.FileManager as FileManager exposing (File)
import Ui.Container
import Ui.Layout
import Ui.Button
import Ui


{-| Representation of an image viewer:
  - **dataURI** - This holds the current image as a dataURI
  - **image** - This holds the current image as a file
  - **size** - This holds the image size
-}
type alias Model =
  { dataURI : Maybe String
  , size : ( Int, Int )
  , image : Maybe File
  }


{-| Messages that an image viewer can receive.
-}
type Msg
  = UpdateSize ( Int, Int )
  | Open (Task Never File)
  | Opened File
  | Read String
  | NoOp


{-| Initializes an image viewer.
-}
init : ( Model, Cmd Msg )
init =
  ( { dataURI = Nothing
    , image = Nothing
    , size = ( 0, 0 )
    }
  , Cmd.none
  )


{-| Updates an image viewer.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
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
          Task.perform Read (FileManager.readAsDataURL image)
      in
        ( { model | image = Just image }, cmd )

    -- Open a file browser to load an image
    Open task ->
      let
        cmd =
          Task.perform Opened task
      in
        ( model, cmd )

    NoOp ->
      ( model, Cmd.none )


{-| Renders an image viewer
-}
view : Model -> Html.Html Msg
view model =
  let
    hasImage =
      Maybe.map (\_ -> "has-image") model.image
        |> Maybe.withDefault ""
  in
    node "image-preview"
      [ class hasImage ]
      [ node "link" [ rel "stylesheet", href "style.css" ] []
      , node "image-preview-header" []
        [ renderHeader
        , node "span"
          [ on "click" (FileManager.openSingleDecoder "image/*" Open) ]
          [ Ui.Button.view NoOp
            { disabled = False
            , readonly = False
            , text = "Load Image"
            , kind = "primary"
            , size = "big"
            }
          ]
        ]
    , node "image-preview-content" [] [ renderImage model ]
    , node "image-preview-info" [] (renderInfo model)
    ]


{-| Decodes an images size.
-}
decodeImageSize : Json.Decoder ( Int, Int )
decodeImageSize =
  Json.map2 (,)
    (Json.at [ "target", "naturalWidth" ] Json.int)
    (Json.at [ "target", "naturalHeight" ] Json.int)


{-| Renders the header.
-}
renderHeader : Html.Html Msg
renderHeader =
  div []
    [ node "h1" []
      [ text "Image Preview" ]
    , node "p" []
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
          ( toString (Tuple.first model.size))
            ++ "px x "
            ++ (toString (Tuple.second model.size)
            ++ "px"
          )
      in
        [ text
            ( "File name: "
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


main : Program Never Model Msg
main =
  Html.program
    { subscriptions = \_ -> Sub.none
    , update = update
    , view = view
    , init = init
    }
