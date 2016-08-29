module Main exposing (..)

import Html.Attributes exposing (style)
import Html exposing (div, text)
import Html.App

import Html.Events.Geometry exposing (Dimensions, onWithDimensions)
import Ui.Helpers.Drag as Drag

type alias Model =
  { position : (Float, Float)
  , startPosition : (Float, Float)
  , drag : Drag.Model
  }

type Msg
  = MouseDown Dimensions
  | MouseMove (Float, Float)
  | Click Bool


init : Model
init =
  { position = (20, 20)
  , startPosition = (0,0)
  , drag = Drag.init
  }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseDown (mousePosition, dimensions, windowSize) ->
      ({ model
       | drag = Drag.lift dimensions mousePosition model.drag
       , startPosition = model.position
       }, Cmd.none)

    MouseMove (left, top) ->
      let
        diff =
          Drag.diff left top model.drag

        position =
          ( (fst model.startPosition) + diff.top
          , (snd model.startPosition) + diff.left)
      in
        ({ model | position = position }, Cmd.none)

    Click pressed ->
      ({ model | drag = Drag.handleClick pressed model.drag }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Drag.subscriptions MouseMove Click model.drag.dragging


view : Model -> Html.Html Msg
view model =
  div
    [ style
      [ ("top", (toString (fst model.position)) ++ "px")
      , ("left", (toString (snd model.position)) ++ "px")
      ]
    , onWithDimensions "mousedown" False MouseDown
    ]
    [ text "Drag ME!!" ]


main =
  Html.App.program
    { init = ( init, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \model ->
        subscriptions model
    }
