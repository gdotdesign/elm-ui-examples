module VideoLibrary.FolderModal where

import Ext.Signal exposing ((>>>))
import Json.Encode as J
import Html

import Ui.Container
import Ui.Button
import Ui.Modal

import VideoLibrary.FolderForm as Form
import VideoLibrary.Types exposing (Folder)

type alias Model =
  { modal: Ui.Modal.Model
  , form : Form.Model
  }

type alias ViewModel a =
  { address : Signal.Address a
  , action : a
  }

type Action
  = Modal Ui.Modal.Action
  | Form Form.Action

init : Model
init =
  { modal = Ui.Modal.init
  , form = Form.init
  }

open : Model -> Model
open model =
  { model | modal = Ui.Modal.open model.modal
          , form = Form.init
  }

openWithFolder : Folder -> Model -> Model
openWithFolder folder model =
  { model | modal = Ui.Modal.open model.modal
          , form = Form.fromFolder folder
  }

close : Model -> Model
close model =
  { model | modal = Ui.Modal.close model.modal }

isNew : Model -> Bool
isNew model =
  Form.isNew model.form

asParams : Model -> List (String, J.Value)
asParams model =
  Form.asParams model.form

update: Action -> Model -> Model
update action model =
  case action of
    Modal act ->
      { model | modal = Ui.Modal.update act model.modal }
    Form act ->
      { model | form = Form.update act model.form }

view: Signal.Address Action -> ViewModel a -> Model -> Html.Html
view address viewModel model =
  let
    (title, button) =
      if Form.isNew model.form then
        ("Add Folder", "Add")
      else
        ("Edit Folder", "Save")
  in
    Ui.Modal.view
      (address >>> Modal)
      { title = title
      , content =
        [ Form.view (address >>> Form) model.form ]
      , footer =
        [ Ui.Container.rowEnd []
          [ Ui.Button.view
            viewModel.address
            viewModel.action
            { kind = "primary"
            , size = "medium"
            , disabled = not (Form.isValid model.form)
            , text = button
            }
          ]
        ]
      }
      model.modal
