module Dashboard exposing (..)

{-| This is the main page of the app:
  - There is a month indicator and arrows in order to change it
  - The spending for a given month is shown
  - The breakdown of the spending is shown by category
-}

import List.Extra
import Numeral

import Date.Extra.Config.Configs as DateConfigs
import Date.Extra.Format exposing (format)
import Ext.Date
import Date

import Html exposing (div, text, table, tr, td)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Html.App

import Ui.Container
import Ui.Header
import Ui

import Types exposing (..)


{-| Representation of a dashboard:
  - **date** - the date for the displayed month
-}
type alias Model =
  { date : Date.Date }


{-| Representation of a view model for a dashboard:
  - **transactions** - the transactions to display
  - **categories** - the categories to display
  - **settings** - the settings
  - **optionsMsg** - the msg to call when clicking the options icon
  - **addMsg** - the msg to call when clicking on the floating action button
-}
type alias ViewModel msg =
  { transactions : List Transaction
  , categories : List Category
  , settings : Settings
  , optionsMsg : msg
  , addMsg : msg
  }


{-| Messages that a dashboard can receive.
-}
type Msg
  = PreviousDate
  | NextDate
  | NoOp


{-| Initializes a dashboard.
-}
init : Model
init =
  { date = Ext.Date.now () }


{-| Updates a dashboard.
-}
update : Msg -> Model -> Model
update action model =
  case action of
    NextDate ->
      { model | date = Ext.Date.nextMonth model.date }

    PreviousDate ->
      { model | date = Ext.Date.previousMonth model.date }

    NoOp ->
      model


{-| Renders a dashboard.
-}
view : (Msg -> msg) -> ViewModel msg -> Model -> Html.Html msg
view address viewModel model =
  let
    {- Transactions in the selected month. -}
    transactions =
      List.filter
        (\transaction -> Ext.Date.isSameMonth transaction.date model.date)
        viewModel.transactions

    {- Spending in the selected month. -}
    spending =
      (viewModel.settings.prefix ++ " ")
        ++ (Numeral.format "0,0" (toFloat (sumSpending transactions)))
        ++ (" " ++ viewModel.settings.affix)

    {- Category list. -}
    breakdown =
      List.map (renderCategory transactions) viewModel.categories

    month =
      format (DateConfigs.getConfig "en_us") "%B, %Y" model.date

    {- Month indicator. -}
    monthIndicator =
      Ui.Container.row
        []
        [ Ui.icon "chevron-left" True [ onClick (address PreviousDate) ]
        , div
            [ class "mt-dashboard-month"]
            [ text month ]
        , Ui.icon "chevron-right" True [ onClick (address NextDate) ]
        ]

    {- Header -}
    header =
      Ui.Header.view
        []
        [ Ui.Header.title "Dashboard" (address NoOp)
        , Ui.spacer
        , Ui.Header.icon
            "android-options"
            viewModel.optionsMsg
        ]
  in
    Ui.Container.column
      []
      [ header
      , Ui.panel
          [ class "mt-dashboard-panel" ]
          [ Ui.Container.column
              []
              [ monthIndicator
              , div
                  [ class "mt-dashboard-spending" ]
                  [ text spending ]
              , Ui.fab "plus" [ onClick viewModel.addMsg ]
              , div [] breakdown
              ]
          ]
      ]


{-| Returns the sum amount of the given transations.
-}
sumSpending : List Transaction -> Int
sumSpending transactions =
  List.map .amount transactions
    |> List.foldr (+) 0


{-| Renders a category.
-}
renderCategory : List Transaction -> Category -> Html.Html msg
renderCategory transactions category =
  let
    categoryTransactions =
      List.filter (\item -> item.categoryId == category.id) transactions

    sum =
      sumSpending categoryTransactions

    row =
      tr []
        [ td [] [ text category.name ]
        , td [] [ text (Numeral.format "0,0" (toFloat sum)) ]
        ]
  in
    div
      [ class "mt-dashboard-category" ]
      [ table
          []
          (row :: (List.map renderTransaction categoryTransactions))
      ]


{-| Renders a transaction.
-}
renderTransaction : Transaction -> Html.Html msg
renderTransaction transaction =
  let
    date =
      format (DateConfigs.getConfig "en_us") "%Y-%m-%d" transaction.date

    amount =
      Numeral.format "0,0" (toFloat transaction.amount)
  in
    tr
      []
      [ td [] [ text date ]
      , td [] [ text amount ]
      ]
