module Main exposing (..)

import Update.Extra.Infix exposing ((:>))
import Debug exposing (log)
import Ext.Date
import Task
import Date

import Html.Attributes exposing (classList)
import Html.Events.Extra exposing (onStop)
import Html exposing (div, text, node)
import Html.Events exposing (onClick)
import Html.App

import Json.Decode as Json
import Json.Encode

import Native.Uid

import Ui.Native.LocalStorage as LocalStorage
import Ui.Container
import Ui.Pager
import Ui.App
import Ui

import Types as Types exposing (..)
import Dashboard as Dashboard
import Settings as Settings
import Form as Form


type Msg
  = Dashboard Dashboard.Msg
  | Settings Settings.Msg
  | Pager Ui.Pager.Msg
  | Form Form.Msg
  | App Ui.App.Msg
  | SelectPage Int
  | Error String
  | Load String
  | SaveStore
  | Saved
  | Save
  | NoOp


{-| Representation of a money tracker application.
-}
type alias Model =
  { dashboard : Dashboard.Model
  , settings : Settings.Model
  , pager : Ui.Pager.Model
  , app : Ui.App.Model
  , form : Form.Model
  , store : Store
  }


{-| Initializes a money tracker.
-}
init : ( Model, Cmd Msg )
init =
  let
    initialCategories =
      [ { id = "0", name = "Transportation", icon = "android-bus" }
      , { id = "1", name = "Food", icon = "android-cart" }
      , { id = "2", name = "Bills", icon = "cash" }
      ]

    initialAccounts =
      [ { initialBalance = 0
        , name = "Bank Card"
        , icon = ""
        , id = "0"
        }
      , { initialBalance = 0
        , name = "Cash"
        , icon = ""
        , id = "1"
        }
      ]

    model =
      { app = Ui.App.init "MoneyTrack"
      , dashboard = Dashboard.init
      , settings = Settings.init
      , pager = Ui.Pager.init 0
      , form = Form.init
      , store =
          { settings = { prefix = "", affix = "" }
          , categories = initialCategories
          , accounts = initialAccounts
          , transactions = []
          }
      }
  in
    ( model, Task.perform Error Load (LocalStorage.getItem "moneytrack-data") )


{-| Updates a money tracker.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    {- Sub components. -}
    Dashboard act ->
      ( { model | dashboard = Dashboard.update act model.dashboard }, Cmd.none )

    Pager act ->
      ( { model | pager = Ui.Pager.update act model.pager }, Cmd.none )

    Form act ->
      let
        ( form, effect ) =
          Form.update act model.form
      in
        ( { model | form = form }, Cmd.map Form effect )

    Settings act ->
      let
        ( settings, effect ) =
          Settings.update act model.settings
      in
        ( updateSettings { model | settings = settings }
        , Cmd.map Settings effect
        )

    App act ->
      let
        ( app, effect ) =
          Ui.App.update act model.app
      in
        ( { model | app = app }, Cmd.map App effect )

    {- Updates -}
    SelectPage page ->
      let
        pager =
          Ui.Pager.select page model.pager

        updatedModel =
          { model | pager = pager }
      in
        case page of
          1 ->
            ( populateForm 0 (Ext.Date.now ()) updatedModel, Cmd.none )

          _ ->
            ( updatedModel, Cmd.none )

    Save ->
      let
        formData =
          Form.data model.store model.form

        transaction data =
          { id = Native.Uid.uid Nothing
          , amount = data.amount
          , date = data.date
          , categoryId = data.categoryId
          , accountId = data.accountId
          , comment = data.comment
          }

        updatedModel =
          { model | store = updatedStore model.store }

        updatedStore store =
          { store | transactions = transactions }

        transactions =
          case formData of
            Just data ->
              model.store.transactions ++ [ transaction data ]

            _ ->
              model.store.transactions
      in
        ( updatedModel, Cmd.none )
          :> update (SelectPage 0)
          :> update SaveStore

    {- Persistence -}
    Load data ->
      let
        store =
          case Json.decodeString decodeStore data of
            Ok decodedStore ->
              let
                _ =
                  log "Info" "Loaded store from local storage."
              in
                decodedStore

            Err message ->
              let
                _ =
                  log "Error decoding store:" message
              in
                model.store
      in
        ( populateSettings { model | store = store }, Cmd.none )

    Saved ->
      let
        _ =
          log "Info" "Store saved in local storage."
      in
        ( model, Cmd.none )

    SaveStore ->
      let
        task =
          LocalStorage.setItem
            "moneytrack-data"
            (Json.Encode.encode 0 (encodeStore model.store))
      in
        ( model, Task.perform Error (\_ -> Saved) task )

    Error message ->
      let
        _ =
          log "Error" message
      in
        ( model, Cmd.none )

    NoOp ->
      ( model, Cmd.none )


{-| Renders a money tracker.
-}
view : Model -> Html.Html Msg
view model =
  let
    settings =
      let
        viewModel =
          { backMsg = SelectPage 0 }
      in
        Settings.view Settings viewModel model.settings

    dashboard =
      let
        viewModel =
          { transactions = model.store.transactions
          , categories = model.store.categories
          , settings = model.store.settings
          , optionsMsg = SelectPage 2
          , addMsg = SelectPage 1
          }
      in
        Dashboard.view Dashboard viewModel model.dashboard

    form =
      let
        bottomLeft =
          div [ onStop "mousedown" (SelectPage 0) ] [ Ui.icon "close" False [] ]

        bottomRight =
          div [ onStop "mousedown" Save ] [ Ui.icon "checkmark" False [] ]

        viewModel =
          { bottomRight = bottomRight
          , bottomLeft = bottomLeft
          , backMsg = SelectPage 0
          }
      in
        Form.view Form viewModel model.form
  in
    Ui.App.view
      App
      model.app
      [ div
          [ classList [ ( "money-track", True ) ] ]
          [ Ui.Pager.view
              Pager
              [ dashboard
              , form
              , settings
              ]
              model.pager
          ]
      ]


{-| Populates the form with the given parameters.
-}
populateForm : Int -> Date.Date -> Model -> Model
populateForm amount date model =
  { model | form = Form.populate model.store amount date model.form }


{-| Populates the settings component from the store.
-}
populateSettings : Model -> Model
populateSettings model =
  { model | settings = Settings.populate model.store.settings model.settings }


{-| Updates settings object from the settings component.
-}
updateSettings : Model -> Model
updateSettings model =
  { model
    | store =
        updateStoreSettings
          { prefix = model.settings.prefix.value
          , affix = model.settings.affix.value
          }
          model.store
  }


gatherSubs : Model -> Sub Msg
gatherSubs model =
  Sub.batch [ Sub.map Form (Form.subscriptions model.form) ]


main : Program Never
main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = gatherSubs
    }
