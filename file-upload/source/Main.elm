{-| This example shows how to select a file and upload it via POST request.
-}
import Html exposing (div, span, text, hr, p, node)
import Html.Attributes exposing (style, href, rel)
import Html.Events exposing (on)

import Task exposing (Task)
import Json.Decode as Json
import Http

import Ui.Native.FileManager as FileManager exposing (File)
import Ui.Button
import Ui


{-| Our model are just the logs.
-}
type alias Model =
  { log : List String }


{-| Messages for our example.
  - Uploaded - this triggers when the request finishes
  - Opened - this triggers when a user selects a file
  - Selected - this triggers when the file is ready to be accessed
  - NoOp - no operation message for the button
-}
type Msg
  = Uploaded (Result Http.Error String)
  | Opened (Task Never File)
  | Selected File
  | NoOp


{-| Initialize our model.
-}
init : Model
init =
  { log = []
  }


{-| Updates for our model.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    -- Get the actual file with a task
    Opened task ->
      ( model, Task.perform Selected task )

    -- Upload the file
    Selected file ->
      ( { model | log = [ "Uploading..." ] }
      , upload file
      )

    -- Upload finished report the response
    Uploaded response ->
      ( { model
          | log =
              model.log
                ++ [ "Uploaded!"
                   , "Response: " ++ (toString response)
                   ]
        }
      , Cmd.none
      )

    NoOp ->
      ( model, Cmd.none )


{-| Uploads the given file.
-}
upload : File -> Cmd Msg
upload file =
  Http.request
    { body = Http.multipartBody [ FileManager.toFormData "file" file ]
    , url = "https://httpbin.org/post"
    , expect = Http.expectString
    , withCredentials = False
    , timeout = Nothing
    , method = "POST"
    , headers = []
    }
    |> Http.send Uploaded


{-| Render our example.
-}
view : Model -> Html.Html Msg
view model =
  let
    logLine line =
      p [] [ text line ]
  in
    div []
      [ node "link" [ rel "stylesheet", href "style.css" ] []
      , span []
          [ text "Select a file and check your network requests!" ]
      , span
          -- Attach a decoder to select a file on click
          [ on "click" (FileManager.openSingleDecoder "image/*" Opened) ]
          [ Ui.Button.view NoOp
            { disabled = False
            , readonly = False
            , kind = "primary"
            , size = "medium"
            , text = "Browse"
            }
          ]
      , hr [] []
      , span [] (List.map logLine model.log)
      ]


main : Program Never Model Msg
main =
  Html.program
    { init = ( init, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \model -> Sub.none
    }
