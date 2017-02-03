module Form exposing (..)

{-| Form for editing / creating transactions.
-}

import List.Extra
import Ext.Date
import Date

import Html.Attributes exposing (classList, class)
import Html.Events exposing (onClick)
import Html exposing (div, text)

import Ui.NumberPad
import Ui.Chooser
import Ui.Header
import Ui.DatePicker
import Ui.Container
import Ui

import Types as Types exposing (..)
import Icons

{-| Messages that a form can receive.
-}
type Msg
  = CategoryChooser Ui.Chooser.Msg
  | AccountChooser Ui.Chooser.Msg
  | DatePicker Ui.DatePicker.Msg
  | NumberPad Ui.NumberPad.Msg


{-| Representation of a form:
  - **categoryChooser** - the chooser for the category
  - **accountChooser** - the chooser for the account
  - **datePicker** - the date picker
  - **numberPad** - the number pad
-}
type alias Model =
  { categoryChooser : Ui.Chooser.Model
  , accountChooser : Ui.Chooser.Model
  , datePicker : Ui.DatePicker.Model
  , numberPad : Ui.NumberPad.Model
  }


{-| Representation of a view model for a form:
  - **bottomRight** - content for the bottom right panel in the number pad
  - **bottomLeft** - content for the bottom left panel in the number pad
  - **backMsg** - the message that will navigate back
-}
type alias ViewModel msg =
  { bottomRight : Html.Html msg
  , bottomLeft : Html.Html msg
  , backMsg : msg
  }


{-| Data model.
-}
type alias Data =
  { categoryId : String
  , accountId : String
  , date : Date.Date
  , comment : String
  , amount : Int
  }


{-| Subscriptions for a form.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map DatePicker (Ui.DatePicker.subscriptions model.datePicker)


{-| Initializes a form.
-}
init : Model
init =
  let
    datePicker =
      Ui.DatePicker.init ()
        |> Ui.DatePicker.setValue (Ext.Date.now ())

    accountChooser =
      Ui.Chooser.init ()
        |> Ui.Chooser.placeholder "Account..."

    categoryChooser =
      Ui.Chooser.init ()
        |> Ui.Chooser.placeholder "Category..."
  in
    { categoryChooser = { categoryChooser | closeOnSelect = True }
    , accountChooser = { accountChooser | closeOnSelect = True }
    , datePicker = { datePicker | closeOnSelect = True }
    , numberPad = Ui.NumberPad.init ()
    }


{-| Populates the form with the given value.
-}
populate : Store -> Int -> Date.Date -> Model -> Model
populate store amount date model =
  let
    mapItem item =
      { id = item.id, value = item.id, label = item.name }

    categories =
      List.map mapItem store.categories

    accounts =
      List.map mapItem store.accounts

    updatedChooser data chooser =
      Ui.Chooser.updateData data chooser
        |> selectFirst

    selectFirst chooser =
      case Ui.Chooser.getFirstSelected chooser of
        Just value ->
          chooser

        _ ->
          Tuple.first (Ui.Chooser.selectFirst chooser)

    numberPad =
      Ui.NumberPad.setValue amount model.numberPad
  in
    { model
      | categoryChooser = updatedChooser categories model.categoryChooser
      , accountChooser = updatedChooser accounts model.accountChooser
      , datePicker = Ui.DatePicker.setValue date model.datePicker
      , numberPad = numberPad
    }


{-| Builds the data object.
-}
buildData : Account -> Category -> Int -> Model -> Data
buildData account category amount model =
  { date = model.datePicker.calendar.value
  , categoryId = category.id
  , accountId = account.id
  , amount = amount
  , comment = ""
  }


{-| Returns the current data from a form.
-}
data : Store -> Model -> Maybe Data
data store model =
  let
    find id_ list =
      Maybe.andThen (\id -> List.Extra.find (\item -> item.id == id) list) id_

    account =
      find (Ui.Chooser.getFirstSelected model.accountChooser) store.accounts

    category =
      find (Ui.Chooser.getFirstSelected model.categoryChooser) store.categories

    amount =
      if model.numberPad.value == 0 then
        Nothing
      else
        Just model.numberPad.value
  in
    Maybe.map4 buildData account category amount (Just model)


{-| Updates a form.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg_ model =
  case msg_ of
    NumberPad msg ->
      let
        ( numberPad, cmd ) =
          Ui.NumberPad.update msg model.numberPad
      in
        ( { model | numberPad = numberPad }
        , Cmd.map NumberPad cmd
        )

    AccountChooser msg ->
      let
        ( accountChooser, cmd ) =
          Ui.Chooser.update msg model.accountChooser
      in
        ( { model | accountChooser = accountChooser }
        , Cmd.map AccountChooser cmd
        )

    CategoryChooser msg ->
      let
        ( categoryChooser, cmd ) =
          Ui.Chooser.update msg model.categoryChooser
      in
        ( { model | categoryChooser = categoryChooser }
        , Cmd.map CategoryChooser cmd
        )

    DatePicker msg ->
      let
        ( datePicker, cmd ) =
          Ui.DatePicker.update msg model.datePicker
      in
        ( { model | datePicker = datePicker }
        , Cmd.map DatePicker cmd
        )


{-| Renders a form.
-}
view : (Msg -> msg) -> ViewModel msg -> Model -> Html.Html msg
view address viewModel model =
  let
    datePicker =
      Html.map
        (address << DatePicker)
        (Ui.DatePicker.view "en_us" model.datePicker)

    accountChooser =
      Html.map
        (address << AccountChooser)
        (Ui.Chooser.view model.accountChooser)

    categoryChooser =
      Html.map
        (address << CategoryChooser)
        (Ui.Chooser.view model.categoryChooser)
  in
    Ui.Container.view
      { direction = "column"
      , align = "stretch"
      , compact = True
      }
      []
      [ Ui.Header.view
          [ Ui.Header.icon
              { action = Just viewModel.backMsg
              , glyph = Icons.back []
              , link = Nothing
              , target = ""
              , size = 32
              }
          , Ui.Header.title
            { text = "Add Transation"
            , action = Nothing
            , target = ""
            , link = Nothing
            }
          ]
      , div
          [ classList [ ( "form", True ) ] ]
          [ Ui.Container.view
              { align = "stretch"
              , direction = "column"
              , compact = False
              }
              []
              [ div [ class "field" ]
                [ div [ class "field-label" ] [ text "Date" ]
                , datePicker
                ]
              , div [ class "field" ]
                [ div [ class "field-label" ] [ text "Account" ]
                , accountChooser
                ]
              , div [ class "field" ]
                [ div [ class "field-label" ] [ text "Category" ]
                , categoryChooser
                ]
              , Ui.NumberPad.view
                  { bottomRight = viewModel.bottomRight
                  , bottomLeft = viewModel.bottomLeft
                  , address = (address << NumberPad)
                  }
                  model.numberPad
              ]
          ]
      ]
