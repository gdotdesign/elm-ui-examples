module Settings exposing (..)

{-| Component for changing the settings, currently affix and prefix.
-}

import Html.Events exposing (onClick)
import Html exposing (text)
import Html.App

import Ui.Container
import Ui.Header
import Ui.Input
import Ui

import Types exposing (Settings)


{-| Representation of a settings component.
  - **affix** - the input for the affix
  - **prefix** - the input for the prefix
-}
type alias Model =
  { affix : Ui.Input.Model
  , prefix : Ui.Input.Model
  }


{-| Representation of a view model for a settings component.
  - **backMsg** - the message that will navigate back
-}
type alias ViewModel msg =
  { backMsg : msg
  }


{-| Messages that a settings component receive.
-}
type Msg
  = Affix Ui.Input.Msg
  | Prefix Ui.Input.Msg


{-| Initializes a settings component.
-}
init : Model
init =
  { affix = Ui.Input.init "" "Affix..."
  , prefix = Ui.Input.init "" "Prefix..."
  }


{-| Updates a settings component.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    Affix act ->
      let
        ( affix, effect ) =
          Ui.Input.update act model.affix
      in
        ( { model | affix = affix }, Cmd.map Affix effect )

    Prefix act ->
      let
        ( prefix, effect ) =
          Ui.Input.update act model.prefix
      in
        ( { model | prefix = prefix }, Cmd.map Prefix effect )


{-| Renders a settings component.
-}
view : (Msg -> msg) -> ViewModel msg -> Model -> Html.Html msg
view address viewModel model =
  let
    affix =
      Html.App.map (address << Affix) (Ui.Input.view model.affix)

    prefix =
      Html.App.map (address << Prefix) (Ui.Input.view model.prefix)

    backIcon =
      Ui.Header.icon "android-arrow-back" False [ onClick viewModel.backMsg ]
  in
    Ui.Container.view
      { align = "stretch"
      , direction = "column"
      , compact = True
      }
      []
      [ Ui.Header.view
          []
          [ backIcon
          , Ui.Header.title [] [ text "Settings" ]
          ]
      , Ui.panel
          []
          [ Ui.Container.render
              { align = "stretch"
              , direction = "column"
              , compact = False
              }
              []
              [ Ui.inputGroup "Currency Affix" affix
              , Ui.inputGroup "Currency Prefix" prefix
              ]
          ]
      ]


{-| Populates the model from the given settings.
-}
populate : Settings -> Model -> Model
populate settings model =
  { model
    | affix = Ui.Input.setValue settings.affix model.affix
    , prefix = Ui.Input.setValue settings.prefix model.prefix
  }
