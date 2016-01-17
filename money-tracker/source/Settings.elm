module Settings where

import Signal exposing (forwardTo)
import Effects

import Html.Events exposing (onClick)
import Html exposing (text)
import Ui.Container
import Ui.Input
import Ui

type alias Model =
  { affix : Ui.Input.Model
  , prefix : Ui.Input.Model
  }

type Action
  = Affix Ui.Input.Action
  | Prefix Ui.Input.Action

type alias ViewModel =
  { backHandler : Html.Attribute
  }

init : Model
init =
  { affix = Ui.Input.init ""
  , prefix = Ui.Input.init ""
  }

update: Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Affix act ->
      let
        (affix, effect) = Ui.Input.update act model.affix
      in
        ({ model | affix = affix }, Effects.map Affix effect)
    Prefix act ->
      let
        (prefix, effect) = Ui.Input.update act model.prefix
      in
        ({ model | prefix = prefix }, Effects.map Prefix effect)

view: Signal.Address Action -> ViewModel -> Model -> Html.Html
view address viewModel model =
  Ui.Container.view { align = "stretch"
                    , direction = "column"
                    , compact = True
                    } []
    [ Ui.header []
      [ Ui.icon "android-arrow-back" False [viewModel.backHandler]
      , Ui.headerTitle [] [text "Settings"]
      ]
    , Ui.panel []
      [ Ui.Container.view { align = "stretch"
                    , direction = "column"
                    , compact = False
                    } []
        [ Ui.inputGroup "Currency Affix" (Ui.Input.view (forwardTo address Affix) model.affix)
        , Ui.inputGroup "Currency Prefix" (Ui.Input.view (forwardTo address Prefix) model.prefix)
        ]
      ]
    ]

populate settings model =
  { model | affix = fst (Ui.Input.setValue settings.affix model.affix)
          , prefix = fst (Ui.Input.setValue settings.prefix model.prefix) }
