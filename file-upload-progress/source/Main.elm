{-| This example shows how to select a file and upload it via POST request and
showing the progres of the upload and download.
-}
import Http.Progress as Progress exposing (Progress(..))
import Http exposing (Request)

import Ui.Native.FileManager exposing (File)
import Ui.FileInput

import Html exposing (text, div)

{-| Our model contains the request, progress and an input for selecting a file.
-}
type alias Model =
  { request: Maybe (Request ())
  , input: Ui.FileInput.Model
  , progress: Progress ()
  , progressId: Int
  }


{-| Messages for our example.
  - FileInput - for the input component
  - Progress - for tracking progress
  - Selected - this triggers when the file is ready to be accessed
-}
type Msg
  = FileInput Ui.FileInput.Msg
  | Progress (Progress ())
  | Selected File


{-| Initialize our model.
-}
init : Model
init =
  { input = Ui.FileInput.init ()
  , request = Nothing
  , progress = None
  , progressId = 0
  }


{-| Update our model.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg_ model =
  case msg_ of
    FileInput msg ->
      let
        ( input, cmd ) = Ui.FileInput.update msg model.input
      in
        ( { model | input = input }, Cmd.map FileInput cmd )


    -- when a file is selected
    Selected file ->
      let
        -- create the multipart body with the file
        body =
          Http.multipartBody
            [ Ui.Native.FileManager.toFormData "file" file
            ]

        -- create a request
        request =
          Http.request
            { expect = Http.expectStringResponse (\_ -> Ok ())
            , url = "https://httpbin.org/post"
            , withCredentials = False
            , timeout = Nothing
            , method = "POST"
            , headers = []
            , body = body
            }
      in
        ( { model
          | progressId = model.progressId + 1
          , request = Just request
          , progress = None
          }
        , Cmd.none
        )

    -- request finished
    Progress (Done ()) ->
      ( { model
        | progress = Done ()
        , request = Nothing
        }
      , Cmd.none
      )

    -- request progress
    Progress progress ->
      ( { model | progress = progress
        }
      , Cmd.none
      )


{-| Subscriptions for our example.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
  let
    -- this subscription tracks the progress of the request
    requestSubscription =
      case model.request of
        Just request ->
          Progress.track (toString model.progressId) Progress request

        Nothing ->
          Sub.none
  in
    Sub.batch
      [ Ui.FileInput.onChange Selected model.input
      , requestSubscription
      ]

{-| Render the example.
-}
view : Model -> Html.Html Msg
view model =
  case model.progress of
    Some { bytes, bytesExpected } ->
      text ("Uloading: " ++ (toString bytes) ++ " of " ++ (toString bytesExpected))

    Fail error ->
      text (toString error)

    _ ->
      Html.map FileInput (Ui.FileInput.view model.input)


main : Program Never Model Msg
main =
  Html.program
    { subscriptions = subscriptions
    , init = ( init, Cmd.none )
    , update = update
    , view = view
    }
