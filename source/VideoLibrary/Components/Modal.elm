module VideoLibrary.Components.Modal where

import Ext.Signal exposing ((>>>))
import Json.Encode as J
import Html

import Ui.Container
import Ui.Button
import Ui.Modal

type alias Model a b c =
  { functions : Functions a b c
  , modal : Ui.Modal.Model
  , form : a
  }

type alias Functions a b c =
  { asParams : a -> List (String, J.Value)
  , update : c -> a -> a
  , fromEntity : b -> a
  , isValid : a -> Bool
  , isNew : a -> Bool
  , init : a
  }

type alias ViewModel a b c =
  { view : Signal.Address c -> b -> Html.Html
  , saveTexts : (String, String)
  , newTexts : (String, String)
  , address : Signal.Address a
  , action : a
  }

type Action a
  = Modal Ui.Modal.Action
  | Form a

init : Functions a b c -> Model a b c
init functions =
  { modal = Ui.Modal.init
  , functions = functions
  , form = functions.init
  }

open : Model a b c -> Model a b c
open model =
  { model | modal = Ui.Modal.open model.modal
          , form = model.functions.init
  }

openWithEntity : b -> Model a b c -> Model a b c
openWithEntity entity model =
  { model | modal = Ui.Modal.open model.modal
          , form = model.functions.fromEntity entity
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

update: Action a -> Model b c a -> Model b c a
update action model =
  case action of
    Modal act ->
      { model | modal = Ui.Modal.update act model.modal }
    Form act ->
      { model | form = model.functions.update act model.form }

view: Signal.Address (Action a) -> ViewModel x c a -> Model c d a -> Html.Html
view address viewModel model =
  let
    (title, button) =
      if model.functions.isNew model.form then
        viewModel.newTexts
      else
        viewModel.saveTexts
  in
    Ui.Modal.view
      (address >>> Modal)
      { title = title
      , content =
        [ viewModel.view (address >>> Form) model.form ]
      , footer =
        [ Ui.Container.rowEnd []
          [ Ui.Button.view
            viewModel.address
            viewModel.action
            { kind = "primary"
            , size = "medium"
            , disabled = not (model.functions.isValid model.form)
            , text = button
            }
          ]
        ]
      }
      model.modal
