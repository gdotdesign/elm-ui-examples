module Main exposing (..)

import Html.Attributes exposing (classList, tabindex)
import Html exposing (div, span, strong, text)
import Html.Events exposing (on)
import Html.App

import Json.Decode as Json
import Mouse

import Ui.Helpers.Dropdown
import Ui


type alias Model =
  { dropdownPosition : String
  , open : Bool
  }


type Msg
  = Open Ui.Helpers.Dropdown.Dimensions
  | Close
  | NoOp


init : Model
init =
  { dropdownPosition = "bottom-right"
  , open = False
  }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Open dimensions ->
      ( Ui.Helpers.Dropdown.openWithDimensions dimensions model, Cmd.none )

    Close ->
      ( Ui.Helpers.Dropdown.close model, Cmd.none )

    NoOp ->
      ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
  let
    classes =
      classList
        [ ( "my-dropdown", True )
        , ( "dropdown-open", model.open )
        ]
  in
    div [ classes ]
      [ span
          [ tabindex 0
          , Ui.Helpers.Dropdown.onWithDimensions "focus" Open
          , on "blur" (Json.succeed Close)
          ]
          [ text "Open" ]
      , Ui.Helpers.Dropdown.view NoOp
          model.dropdownPosition
          [ text "Dropdown Contents" ]
      ]


main =
  Html.App.program
    { init = ( init, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
