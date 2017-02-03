{-| This is an example showing how to scroll to the top of the page Elm-UI.
-}
import Html exposing (div, span, strong, text)
import Html.Attributes exposing (style)

import Ui.Button

-- This is from https://github.com/gdotdesign/elm-dom
import DOM

{-| Our message.
-}
type Msg
  = ToTop


{-| Our update.
-}
update : Msg -> {} -> {}
update msg model =
  case msg of
    ToTop ->
      -- We can synchronously scroll and get a result
      case DOM.setScrollTopSync 0 "body" of
        Ok () -> model
        Err error -> model


{-| Our view.
-}
view : {} -> Html.Html Msg
view model =
  div
  [ style
    [ ( "font-family", "sans" )
    , ( "margin", "40px" )
    ]
  ]
  [ span [ ] [ text "Scroll down and push the button!" ]
  , div
    [ style [ ( "padding-top", "1000px" ) ] ]
    [ Ui.Button.view ToTop
      { disabled = False
      , readonly = False
      , text = "To Top!"
      , kind = "primary"
      , size = "medium"
      }
    ]
  ]


main : Program Never {} Msg
main =
  Html.beginnerProgram
    { model = {}
    , view = view
    , update = update
    }
