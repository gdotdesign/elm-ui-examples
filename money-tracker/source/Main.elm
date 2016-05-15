module Main exposing (..)

import Ui.Native.LocalStorage as LocalStorage
import Ext.Date
import Task

import Html.Attributes exposing (classList)
import Html.Events.Extra exposing (onStop)
import Html.Events exposing (onClick)
import Html exposing (div, text, node)
import Html.App

import Json.Encode
import Json.Decode as Json
import Native.Uid

import Ui.Container
import Ui.App
import Ui.Pager
import Ui

import Debug exposing (log)

import Types as Types exposing (..)
import Dashboard as Dashboard
import Settings as Settings
import Form as Form

type Action
  = App Ui.App.Msg
  | Pager Ui.Pager.Msg
  | Dashboard Dashboard.Msg
  | Settings Settings.Action
  | Form Form.Msg
  | SelectPage Int
  | Load String
  | Error String
  | Saved
  | Save
  | NoOp

initialCategories : List Category
initialCategories =
  [ { id = "0", name = "Bills", icon = "cash" }
  , { id = "1", name = "Transportation", icon = "android-bus" }
  , { id = "2", name = "Food", icon = "android-cart" }
  ]

populateForm date amount model =
  { model | form = Form.populate model.store date amount model.form }

populateSettings model =
  { model | settings = Settings.populate model.store.settings model.settings }

type alias Model =
  { app : Ui.App.Model
  , pager : Ui.Pager.Model
  , dashboard : Dashboard.Model
  , settings : Settings.Model
  , form : Form.Model
  , store : Store
  }

init : (Model, Cmd Action)
init =
  ({ app = Ui.App.init "MoneyTrack"
   , pager = Ui.Pager.init 0
   , dashboard = Dashboard.init
   , settings = Settings.init
   , form = Form.init
   , store = { categories = initialCategories
             , transactions = []
             , settings = { prefix = "", affix = "" }
             , accounts = [ { id = "0"
                            , initialBalance = 0
                            , name = "Bank Card"
                            , icon = ""
                            }
                          , { id = "1"
                            , initialBalance = 0
                            , name = "Cash"
                            , icon = ""
                            }
                          ]
             }
   }, Task.perform Error Load (LocalStorage.getItem "moneytrack-data"))

view model =
  Ui.App.view App model.app
    [ div [classList [("money-track", True)]]
      [ Ui.Pager.view
          Pager
          [ dashboard model
          , form model
          , settings model
          ]
          model.pager
      ]
    ]

settings model =
  let
    viewModel =
      { backHandler = onClick (SelectPage 0) }
  in
    Settings.view Settings viewModel model.settings

dashboard model =
  let
    viewModel =
      { optionsMsg = SelectPage 2
      , addMsg = SelectPage 1
      , transactions = model.store.transactions
      , settings = model.store.settings
      , categories = model.store.categories
      }
  in
    Dashboard.view Dashboard viewModel model.dashboard

form model =
  let
    viewModel =
      { bottomLeft = div [onStop "mousedown" (SelectPage 0)] [Ui.icon "close" False []]
      , bottomRight = div [onStop "mousedown" Save] [Ui.icon "checkmark" False []]
      , backMsg = SelectPage 0
      }
  in
    Form.view Form viewModel model.form

updateSettings model =
  { model | store = updateStoreSettings { affix = model.settings.affix.value
                                        , prefix = model.settings.prefix.value
                                        } model.store }

saveStore model =
  let
    task =
      LocalStorage.setItem
        "moneytrack-data"
        (Json.Encode.encode 0 (storeEncoder model.store))
  in
    Task.perform Error (\_ -> Saved) task

update action model =
  case action of
    Form act ->
      let
        (form, effect) = Form.update act model.form
      in
        ({ model | form = form }, Cmd.map Form effect)
    Dashboard act ->
      ({ model | dashboard = Dashboard.update act model.dashboard }, Cmd.none)
    Settings act ->
      let
        (settings, effect) = Settings.update act model.settings
      in
      ({ model | settings = settings }
         |> updateSettings, Cmd.batch [ Cmd.map Settings effect
                                      , saveStore model])
    App act ->
      let
        (app, effect) = Ui.App.update act model.app
      in
        ({ model | app = app }, Cmd.map App effect)
    Pager act ->
      ({ model | pager = Ui.Pager.update act model.pager }, Cmd.none)

    SelectPage page ->
      (selectPage page model, Cmd.none)

    Save ->
      let
        formData = Form.data model.store model.form

        transaction data =
          { id = Native.Uid.uid Nothing
          , amount = data.amount
          , date = data.date
          , categoryId = data.categoryId
          , accountId = data.accountId
          , comment = data.comment
          }

        updatedStore store =
          { store | transactions = transactions }

        transactions =
          case formData of
            Just data ->
              model.store.transactions ++ [transaction data]
            _ -> model.store.transactions
      in
        ({ model | store = updatedStore model.store }
        |> selectPage 0, saveStore model)

    Load data ->
      let
        store =
          case Json.decodeString storeDecoder data of
            Ok s -> s
            Err msg -> log msg model.store
      in
        ({ model | store = store } |> populateSettings, Cmd.none)

    Saved ->
      (log "Saved..." model, Cmd.none)
    _ ->
      (model, Cmd.none)

selectPage page model =
  let
    pager =
      Ui.Pager.select page model.pager

    updatedModel =
      { model | pager = pager }
  in
    case page of
      1 ->
        populateForm 0 (Ext.Date.now ()) updatedModel
      _ ->
        updatedModel

main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \model -> Sub.batch [Sub.map Form (Form.subscriptions model.form)]
    }
