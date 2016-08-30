module Main exposing (..)

import Html exposing (div, span, strong, text)
import Html.Attributes exposing (style)
import Html.App

import Ui.Button

import Dom.Scroll
import Task
import Dom


type alias Model =
  {}


type Msg
  = ToTop
  | Scrolled ()
  | NotFound Dom.Error


init : Model
init =
  {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToTop ->
      let
        task =
          Dom.Scroll.toTop "body"
      in
        ( model, Task.perform NotFound Scrolled task )

    Scrolled () ->
      ( model, Cmd.none )

    NotFound _ ->
      ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
  div
    [ style
        [ ( "margin", "40px" )
        , ( "padding-top", "1000px" )
        ]
    ]
    [ Ui.Button.primary "To Top!" ToTop
    ]


main =
  Html.App.program
    { init = ( init, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
