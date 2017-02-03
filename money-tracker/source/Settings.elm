module Settings exposing (..)

{-| Component for changing the settings, currently affix and prefix.
-}

import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html exposing (div, text)

import Ui.Container
import Ui.Header
import Ui.Input
import Ui

import Types exposing (Settings)
import Icons

{-| Representation of a settings component.
  - **prefix** - the input for the prefix
  - **affix** - the input for the affix
-}
type alias Model =
  { prefix : Ui.Input.Model
  , affix : Ui.Input.Model
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
  = Prefix Ui.Input.Msg
  | Affix Ui.Input.Msg


{-| Initializes a settings component.
-}
init : Model
init =
  { affix =
      Ui.Input.init ()
        |> Ui.Input.placeholder "Affix..."
  , prefix =
      Ui.Input.init ()
        |> Ui.Input.placeholder "Prefix..."
  }


{-| Updates a settings component.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg_ model =
  case msg_ of
    Affix msg ->
      let
        ( affix, cmd ) =
          Ui.Input.update msg model.affix
      in
        ( { model | affix = affix }, Cmd.map Affix cmd )

    Prefix msg ->
      let
        ( prefix, cmd ) =
          Ui.Input.update msg model.prefix
      in
        ( { model | prefix = prefix }, Cmd.map Prefix cmd )


{-| Renders a settings component.
-}
view : (Msg -> msg) -> ViewModel msg -> Model -> Html.Html msg
view address viewModel model =
  let
    affix =
      Html.map (address << Affix) (Ui.Input.view model.affix)

    prefix =
      Html.map (address << Prefix) (Ui.Input.view model.prefix)

    backIcon =
      Ui.Header.icon
        { action = Just viewModel.backMsg
        , glyph = Icons.back []
        , link = Nothing
        , size = 32
        , target = ""
        }
  in
    Ui.Container.view
      { direction = "column"
      , align = "stretch"
      , compact = True
      }
      []
      [ Ui.Header.view
          [ backIcon
          , Ui.Header.title
            { text = "Settings"
            , action = Nothing
            , link = Nothing
            , target = ""
            }
          ]
      , div
          [ class "panel" ]
          [ Ui.Container.render
              { direction = "column"
              , align = "stretch"
              , compact = False
              }
              []
              [ div [ class "field" ]
                [ div [ class "field-label" ] [ text "Currency affix" ]
                , affix
                ]
              , div [ class "field" ]
                [ div [ class "field-label" ] [ text "Currency prefix" ]
                , prefix
                ]
              ]
          ]
      ]


{-| Populates the model from the given settings.
-}
populate : Settings -> Model -> ( Model, Cmd Msg)
populate settings model =
  let
    ( affix, affixCmd ) =
      Ui.Input.setValue settings.affix model.affix

    ( prefix, prefixCmd ) =
      Ui.Input.setValue settings.prefix model.prefix
  in
    ( { model | affix = affix, prefix = prefix }
    , Cmd.batch
      [ Cmd.map Affix affixCmd
      , Cmd.map Prefix prefixCmd
      ]
    )
