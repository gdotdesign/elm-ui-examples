module Form where

{- Form for editing / creating transactions. -}
import Signal exposing (forwardTo)
import List.Extra
import Ext.Date
import Effects
import Date


import Html.Attributes exposing (classList)
import Html exposing (text)

import Ui.NumberPad
import Ui.Chooser
import Ui.Header
import Ui.DatePicker
import Ui.Container
import Ui

import Types as Types exposing (..)

type Action
  = NumberPad Ui.NumberPad.Action
  | AccountChooser Ui.Chooser.Action
  | CategoryChooser Ui.Chooser.Action
  | DatePicker Ui.DatePicker.Action

type alias Model =
  { categoryChooser : Ui.Chooser.Model
  , accountChooser : Ui.Chooser.Model
  , datePicker : Ui.DatePicker.Model
  , numberPad : Ui.NumberPad.Model
  }

type alias ViewModel =
  { bottomRight : Html.Html
  , bottomLeft : Html.Html
  , backHandler : Html.Attribute
  }

type alias Data =
  { categoryId : String
  , accountId : String
  , date : Date.Date
  , comment : String
  , amount : Int
  }

init : Signal.Address Action -> Model
init address =
  let
    datePicker =
      Ui.DatePicker.init (forwardTo address DatePicker) (Ext.Date.now ())

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

populate : Store -> Int -> Date.Date -> Model -> Model
populate store amount date model =
  let
    mapItem item = { value = item.id, label = item.name }
    categories = List.map mapItem store.categories
    accounts = List.map mapItem store.accounts

    updatedChooser data chooser =
      Ui.Chooser.updateData data chooser
        |> selectFirst

    selectFirst chooser =
      case Ui.Chooser.getFirstSelected chooser of
        Just value -> chooser
        _ ->
          fst (Ui.Chooser.selectFirst chooser)

    (numberPad, effect) = Ui.NumberPad.setValue amount model.numberPad
  in
    { model | categoryChooser = updatedChooser categories model.categoryChooser
            , accountChooser  = updatedChooser accounts model.accountChooser
            , datePicker = Ui.DatePicker.setValue date model.datePicker
            , numberPad = numberPad
            }

buildData : Account -> Category -> Int -> Model -> Data
buildData account category amount model =
  { categoryId = category.id
  , accountId  = account.id
  , date       = model.datePicker.calendar.value
  , amount     = amount
  , comment    = ""
  }

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

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    NumberPad act ->
      let
        (numberPad, effect) = Ui.NumberPad.update act model.numberPad
      in
        ({ model | numberPad = numberPad }, Effects.map NumberPad effect)
    AccountChooser act ->
      let
        (accountChooser, effect) = Ui.Chooser.update act model.accountChooser
      in
        ({ model | accountChooser = accountChooser }, Effects.map AccountChooser effect)
    CategoryChooser act ->
      let
        (categoryChooser, effect) = Ui.Chooser.update act model.categoryChooser
      in
        ({ model | categoryChooser = categoryChooser }, Effects.map CategoryChooser effect)
    DatePicker act ->
      let
        (datePicker, effect) = Ui.DatePicker.update act model.datePicker
      in
        ({ model | datePicker = datePicker }, Effects.map DatePicker effect)

view : Signal.Address Action -> ViewModel -> Model -> Html.Html
view address viewModel model =
  let
    datePicker =
      Ui.DatePicker.view (forwardTo address DatePicker) model.datePicker

    accountChooser =
      Ui.Chooser.view (forwardTo address AccountChooser) model.accountChooser

    categoryChooser =
      Ui.Chooser.view (forwardTo address CategoryChooser) model.categoryChooser
  in
    Ui.Container.view { align = "stretch"
                      , direction = "column"
                      , compact = True
                      } []
      [ Ui.Header.view []
        [ Ui.Header.icon "android-arrow-back" False [viewModel.backHandler]
        , Ui.Header.title [] [text "Edit Transaction"]
        ]
      , Ui.panel [classList [("money-track-form", True)]]
        [ Ui.Container.view { align = "stretch"
                          , direction = "column"
                          , compact = False
                          } []
          [ Ui.inputGroup "Date" datePicker
          , Ui.inputGroup "Account" accountChooser
          , Ui.inputGroup "Category" categoryChooser
          , Ui.NumberPad.view
              { bottomLeft = viewModel.bottomLeft
              , bottomRight = viewModel.bottomRight
              }
              (forwardTo address NumberPad)
              model.numberPad
          ]
        ]
      ]
