module Main exposing (..)

{-| This example shows how to implement a simple dropdown using
Ui.Helpers.Dropdown.
-}
import Html exposing (node, button, text, div)
import Html.Attributes exposing (href, rel)
import Html.Events exposing (onClick)

import Ui.Helpers.Dropdown as Dropdown
import Ui

{-| The model for our dropdown.
-}
type alias Model =
  { dropdown : Dropdown.Dropdown
  , uid : String
  }


{-| Messages for our dropdown.
-}
type Msg
  = Dropdown Dropdown.Msg
  | Open


{-| Initialize our dropdown.
-}
init : Model
init =
  ( { dropdown = Dropdown.init
    , uid = "my-dropdown"
    }
    |> Dropdown.offset 5
  , Cmd.none
  )


{-| Updates for our dropdown.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Open ->
      ( Dropdown.open model, Cmd.none )

    Dropdown msg ->
      ( Dropdown.update msg model, Cmd.none )


{-| Render a link for the styles and our dropdown.
-}
view : Model -> Html.Html Msg
view model =
  div []
    [ node "link" [ rel "stylesheet", href "style.css" ] []
    , Dropdown.view
      { children = [ button [ onClick Open ] [ text "Open" ] ]
      , contents = [ text "Contents..." ]
      , address = Dropdown
      , attributes = []
      , tag = "span"
      }
      model
    ]


{-| Subscriptions for our dropdown.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map Dropdown (Dropdown.subscriptions model)


main : Program Never Model Msg
main =
  Html.program
    { subscriptions = subscriptions
    , update = update
    , view = view
    , init = init
    }
