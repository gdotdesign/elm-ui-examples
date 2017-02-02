module Components.Modal exposing (..)

import Update.Extra.Infix exposing ((:>))
import Json.Encode as J
import Html

import Ui.Helpers.Emitter as Emitter
import Ui.Container
import Ui.Button
import Ui.Modal

type alias Model model entity msg =
  { functions : Functions model entity msg
  , modal : Ui.Modal.Model
  , form : model
  , entity : Maybe entity
  , parentId : Int
  , loading : Bool
  }

type alias Functions model entity msg =
  { asParams : model -> List (String, J.Value)
  , update : msg -> model -> (model, Cmd msg)
  , fromEntity : entity -> model
  , isValid : model -> Bool
  , isNew : model -> Bool
  , init : model
  , id : entity -> Int
  , editTexts: (String, String)
  , newTexts: (String, String)
  , view : model -> Html.Html msg
  , get : Int -> (Result String entity -> Action entity msg) -> Cmd (Action entity msg)
  , patch : Int -> List (String, J.Value) -> (Result String entity -> Action entity msg) -> Cmd (Action entity msg)
  , create : List (String, J.Value) -> (Result String entity -> Action entity msg ) -> Cmd (Action entity msg)
  }

type Action entity msg
  = Modal Ui.Modal.Msg
  | Form msg
  | Save
  | Load Int
  | Loaded (Result String entity)
  | Saved (Result String entity)
  | FinishLoading
  | StartLoading
  | Error String
  | Create Int

init : Functions model entity msg -> Model model entity msg
init functions =
  { modal = Ui.Modal.init
  , functions = functions
  , form = functions.init
  , entity = Nothing
  , parentId = 0
  , loading = False
  }

subscriptions : String -> Sub (Action entity msg)
subscriptions channel =
  Sub.batch [ Emitter.listenInt ("edit-" ++ channel) Load
            , Emitter.listenInt ("create-" ++ channel) Create
            ]

open : Int -> Model a b c -> Model a b c
open parentId model =
  { model | modal = Ui.Modal.open model.modal
          , form = model.functions.init
          , parentId = parentId
  }

openWithEntity : entity -> Model model entity msg -> Model model entity msg
openWithEntity entity model =
  { model | modal = Ui.Modal.open model.modal
          , form = model.functions.fromEntity entity
          , entity = Just entity
  }

close : Model a b c -> Model a b c
close model =
  { model | modal = Ui.Modal.close model.modal }

isNew : Model a b c -> Bool
isNew model =
  model.functions.isNew model.form

asParams : Model a b c -> List (String, J.Value)
asParams model =
  model.functions.asParams model.form

update: Action entity msg -> Model model entity msg -> (Model model entity msg, Cmd (Action entity msg))
update action model =
  case action of
    Save ->
      let
        params =
          asParams model

        createParams =
          params ++ [("folder_id", J.int model.parentId)]

        cmd =
          Maybe.map (\entity -> model.functions.patch (model.functions.id entity) params Saved) model.entity
          |> Maybe.withDefault (model.functions.create createParams Saved)
      in
        (model, cmd)
        :> update StartLoading

    Create parentId ->
      (open parentId model, Cmd.none)

    Load id ->
      (model, model.functions.get id Loaded)
      :> update StartLoading

    Loaded result ->
      case result of
        Ok entity ->
          (openWithEntity entity model, Cmd.none)
          :> update FinishLoading
        Err message ->
          update (Error message) model

    StartLoading ->
      ({ model | loading = True }, Cmd.none)

    FinishLoading ->
      ({ model | loading = False }, Cmd.none)

    Saved result ->
      case result of
        Ok entity ->
          (close model, Emitter.sendNaked "refresh")
          :> update FinishLoading
        Err message ->
          update (Error message) model

    Error message ->
      (model, Emitter.sendString "errors" message)
      :> update FinishLoading

    Modal act ->
      ({ model | modal = Ui.Modal.update act model.modal }, Cmd.none)

    Form act ->
      let
        (form, cmd) = model.functions.update act model.form
      in
        ({ model | form = form }, Cmd.map Form cmd)

view: Model model entity msg -> Html.Html (Action entity msg)
view model =
  let
    (title, button) =
      if model.functions.isNew model.form then
        model.functions.newTexts
      else
        model.functions.editTexts
  in
    Ui.Modal.view
      Modal
      { title = title
      , content =
        [ Html.map Form (model.functions.view model.form) ]
      , footer =
        [ Ui.Container.rowEnd []
          [ Ui.Button.view
            Save
            { kind = "primary"
            , size = "medium"
            , disabled = (not (model.functions.isValid model.form) || model.loading)
            , readonly = False
            , text = button
            }
          ]
        ]
      }
      model.modal
