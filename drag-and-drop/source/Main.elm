{-| This is an example how to implement a simple drag and drop functionanily
with Ui.Helpers.Drag.
-}
import Html.Attributes exposing (style)
import Html exposing (div, text)

import DOM exposing (Position)

import Ui.Helpers.Drag as Drag


{-| The model:
  - startPosition - the position of the box when the drag starts
  - uid - the unique id for the drag (not used in this example)
  - position - the current position of the box
  - drag - the model for the Drag
-}
type alias Model =
  { startPosition : Position
  , position : Position
  , drag : Drag.Drag
  , uid : String
  }


{-| The messages:
  - Lift - triggered when the mouse is down
  - Move - triggered when the mouse is moving
  - End - triggered when the mouse is up
-}
type Msg
  = Lift Position
  | Move Position
  | End


{-| Initialize the model.
-}
init : ( Model, Cmd Msg )
init =
  ( { startPosition = { left = 0, top = 0 }
    , position = { left = 0, top = 0 }
    , drag = Drag.init
    , uid = "drag"
    }
  , Cmd.none
  )


{-| Update the model when something happens.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Lift position ->
      let
        -- Set the start position and start the drag with Drag.lift
        updatedModel =
          { model | startPosition = model.position
          }
          |> Drag.lift position
      in
       ( updatedModel, Cmd.none )

    Move position ->
      let
        -- Get the diff between the start position and the current position
        diff =
          Drag.diff position model

        -- Calculate new positions
        newPosition =
          { left = model.startPosition.left + diff.left
          , top = model.startPosition.top + diff.top
          }
      in
        ( { model | position = newPosition }, Cmd.none )

    End ->
      -- End the drag
      ( Drag.end model, Cmd.none)


{-| Subscriptions for the drag.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Drag.onMove Move model
    , Drag.onEnd End model
    ]


{-| The view.
-}
view : Model -> Html.Html Msg
view model =
  div
    [ style
      [ ("left", (toString model.position.left) ++ "px")
      , ("top", (toString model.position.top) ++ "px")

      , ("border", "1px solid #DDD")
      , ("background", "#f5f5f5")
      , ("position", "absolute")
      , ("font-family", "sans")
      , ("padding", "40px")
      , ("cursor", "move")

      , ("-webkit-user-select", "none")
      , ("-moz-user-select", "none")
      , ("-ms-user-select", "none")
      , ("user-select", "none")
      ]
    -- Attach the lift handler, this decodes the mouse the position
    , Drag.liftHandler Lift
    ]
    [ text "Drag ME!!" ]


main : Program Never Model Msg
main =
  Html.program
    { subscriptions = subscriptions
    , update = update
    , view = view
    , init = init
    }
