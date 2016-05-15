module Form exposing (..)

{-| Form for editing / creating transactions.
-}

import List.Extra
import Ext.Date
import Date

import Html.Attributes exposing (classList)
import Html.Events exposing (onClick)
import Html exposing (text)
import Html.App

import Ui.NumberPad
import Ui.Chooser
import Ui.Header
import Ui.DatePicker
import Ui.Container
import Ui

import Types as Types exposing (..)


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
      Ui.DatePicker.init (Ext.Date.now ())

    accountChooser =
      Ui.Chooser.init [] "Account..." ""

    categoryChooser =
      Ui.Chooser.init [] "Category..." ""
  in
    { categoryChooser = { categoryChooser | closeOnSelect = True }
    , accountChooser = { accountChooser | closeOnSelect = True }
    , datePicker = { datePicker | closeOnSelect = True }
    , numberPad = Ui.NumberPad.init 0
    }


{-| Populates the form with the given value.
-}
populate : Store -> Int -> Date.Date -> Model -> Model
populate store amount date model =
  let
    mapItem item =
      { value = item.id, label = item.name }

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
          fst (Ui.Chooser.selectFirst chooser)

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
    find id' list =
      Maybe.andThen id' (\id -> List.Extra.find (\item -> item.id == id) list)

    account' =
      find (Ui.Chooser.getFirstSelected model.accountChooser) store.accounts

    category' =
      find (Ui.Chooser.getFirstSelected model.categoryChooser) store.categories

    amount' =
      if model.numberPad.value == 0 then
        Nothing
      else
        Just model.numberPad.value
  in
    Maybe.map4 buildData account' category' amount' (Just model)


{-| Updates a form.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NumberPad act ->
      let
        ( numberPad, effect ) =
          Ui.NumberPad.update act model.numberPad
      in
        ( { model | numberPad = numberPad }
        , Cmd.map NumberPad effect
        )

    AccountChooser act ->
      let
        ( accountChooser, effect ) =
          Ui.Chooser.update act model.accountChooser
      in
        ( { model | accountChooser = accountChooser }
        , Cmd.map AccountChooser effect
        )

    CategoryChooser act ->
      let
        ( categoryChooser, effect ) =
          Ui.Chooser.update act model.categoryChooser
      in
        ( { model | categoryChooser = categoryChooser }
        , Cmd.map CategoryChooser effect
        )

    DatePicker act ->
      let
        ( datePicker, effect ) =
          Ui.DatePicker.update act model.datePicker
      in
        ( { model | datePicker = datePicker }
        , Cmd.map DatePicker effect
        )


{-| Renders a form.
-}
view : (Msg -> msg) -> ViewModel msg -> Model -> Html.Html msg
view address viewModel model =
  let
    datePicker =
      Html.App.map
        (address << DatePicker)
        (Ui.DatePicker.view "en_us" model.datePicker)

    accountChooser =
      Html.App.map
        (address << AccountChooser)
        (Ui.Chooser.view model.accountChooser)

    categoryChooser =
      Html.App.map
        (address << CategoryChooser)
        (Ui.Chooser.view model.categoryChooser)
  in
    Ui.Container.render
      { align = "stretch"
      , direction = "column"
      , compact = True
      }
      []
      [ Ui.Header.view
          []
          [ Ui.Header.icon
              "android-arrow-back"
              False
              [ onClick viewModel.backMsg ]
          , Ui.Header.title [] [ text "Add Transation" ]
          ]
      , Ui.panel
          [ classList [ ( "money-track-form", True ) ] ]
          [ Ui.Container.view
              { align = "stretch"
              , direction = "column"
              , compact = False
              }
              []
              [ Ui.inputGroup "Date" datePicker
              , Ui.inputGroup "Account" accountChooser
              , Ui.inputGroup "Category" categoryChooser
              , Ui.NumberPad.view
                  { bottomLeft = viewModel.bottomLeft
                  , bottomRight = viewModel.bottomRight
                  }
                  (address << NumberPad)
                  model.numberPad
              ]
          ]
      ]
