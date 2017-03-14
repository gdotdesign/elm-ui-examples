import Ui.Helpers.Dropdown as Dropdown
import Ui.DropdownMenu
import Ui.Layout
import Ui.Header
import Ui.Icons

import Html.Attributes exposing (href, rel)
import Html exposing (text, div, node)
import Html.Events exposing (onClick)

{-| The model for our example.
-}
type alias Model =
  { dropdown : Ui.DropdownMenu.Model
  }


{-| Initializes a model.
-}
init : Model
init =
  { dropdown =
      Ui.DropdownMenu.init ()
        |> Dropdown.alignTo Dropdown.Right
  }


{-| Messages for our example.
-}
type Msg
  = Dropdown Ui.DropdownMenu.Msg
  | Close
  | Open


{-| Update the model.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Open ->
      ( { model | dropdown = Dropdown.open model.dropdown }, Cmd.none )

    Close ->
      ( { model | dropdown = Dropdown.close model.dropdown }, Cmd.none)

    Dropdown msg ->
      let
        dropdown = Ui.DropdownMenu.update msg model.dropdown
      in
        ( { model | dropdown = dropdown }, Cmd.none )


{-| Subscriptions for our example.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map Dropdown (Ui.DropdownMenu.subscriptions model.dropdown)


{-| The view for our example.
-}
view : Model -> Html.Html Msg
view model =
  div []
    [ node "link" [ rel "stylesheet", href "style.css" ] []
    , Ui.Layout.website
      [ Ui.Header.view
        [ Ui.Header.title
          { text = "Header & drop-down integration"
          , action = Nothing
          , link = Nothing
          , target = ""
          }
        , Ui.Header.spacer
        , Ui.DropdownMenu.view
          { address = Dropdown
          , element =
            Ui.Header.icon
              { glyph = Ui.Icons.search []
              , action = Just Open
              , link = Nothing
              , target = ""
              , size = 16
              }
          , items =
            [ Ui.DropdownMenu.item
              [ onClick Close ]
              [ Ui.Icons.close []
              , text "Close"
              ]
            ]
          }
          model.dropdown
        ]
      ]
      []
      []
    ]


main : Program Never Model Msg
main =
  Html.program
    { subscriptions = subscriptions
    , init = ( init, Cmd.none )
    , update = update
    , view = view
    }
