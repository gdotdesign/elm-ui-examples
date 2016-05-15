module Settings exposing (..)

import Html.Events exposing (onClick)
import Html exposing (text)
import Html.App

import Ui.Container
import Ui.Header
import Ui.Input
import Ui

type alias Model =
  { affix : Ui.Input.Model
  , prefix : Ui.Input.Model
  }

type Action
  = Affix Ui.Input.Msg
  | Prefix Ui.Input.Msg

type alias ViewModel msg =
  { backHandler : Html.Attribute msg
  }

init : Model
init =
  { affix = Ui.Input.init "" "Affix..."
  , prefix = Ui.Input.init "" "Prefix..."
  }

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    Affix act ->
      let
        (affix, effect) = Ui.Input.update act model.affix
      in
        ({ model | affix = affix }, Cmd.map Affix effect)
    Prefix act ->
      let
        (prefix, effect) = Ui.Input.update act model.prefix
      in
        ({ model | prefix = prefix }, Cmd.map Prefix effect)

view : (Action -> msg) -> ViewModel msg -> Model -> Html.Html msg
view address viewModel model =
  Ui.Container.render { align = "stretch"
                      , direction = "column"
                      , compact = True
                      } []
    [ Ui.Header.view []
      [ Ui.Header.icon "android-arrow-back" False [viewModel.backHandler]
      , Ui.Header.title [] [text "Settings"]
      ]
    , Ui.panel []
      [ Ui.Container.render { align = "stretch"
                    , direction = "column"
                    , compact = False
                    } []
        [ Ui.inputGroup "Currency Affix" (Html.App.map (address << Affix) (Ui.Input.view model.affix))
        , Ui.inputGroup "Currency Prefix" (Html.App.map (address << Prefix) (Ui.Input.view model.prefix))
        ]
      ]
    ]

populate settings model =
  { model | affix = Ui.Input.setValue settings.affix model.affix
          , prefix = Ui.Input.setValue settings.prefix model.prefix }
