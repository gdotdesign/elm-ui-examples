module Main exposing (..)

import Html exposing (div, span, text, hr, p)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Html.App

import Task exposing (Task)
import Debug
import Http

import Ui.Native.FileManager as FileManager exposing (File)
import Ui.Button
import Ui


type alias Model =
  { log : List String }


type Msg
  = Opened (Task Never File)
  | Selected File
  | Uploaded Http.Response
  | NoOp


init : Model
init =
  { log = []
  }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Opened task ->
      ( model, Task.perform (\_ -> Debug.crash "") Selected task )

    Selected file ->
      ( { model | log = [ "Uploading..." ] }, upload file )

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


upload : File -> Cmd Msg
upload file =
  let
    body =
      Http.multipart [ FileManager.toFormData "file" file ]

    request =
      Http.send Http.defaultSettings
        { verb = "POST"
        , headers = []
        , url = "https://httpbin.org/post"
        , body = body
        }
  in
    Task.perform (\_ -> Debug.crash "") Uploaded request


view : Model -> Html.Html Msg
view model =
  let
    logLine line =
      p [] [ text line ]
  in
    Ui.panel []
      [ span
          [ style [ ( "padding-right", "15px" ) ] ]
          [ text "Select a file and check your network requests!" ]
      , span
          [ on "click" (FileManager.openSingleDecoder "image/*" Opened) ]
          [ Ui.Button.primary "Browse" NoOp
          ]
      , hr [] []
      , span []
          (List.map logLine model.log)
      ]


main =
  Html.App.program
    { init = ( init, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \model -> Sub.none
    }
