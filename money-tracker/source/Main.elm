module Main exposing (..)

{-| This is an example of a medium app, that has multiple pages for multiple
features.
-}
import Update.Extra.Infix exposing ((:>))
import Debug exposing (log)
import Ext.Date
import Task
import Date

import Html.Attributes exposing (classList, href, rel)
import Html.Events exposing (onClick, onMouseDown)
import Html exposing (div, text, node)

import Json.Decode as Json
import Json.Encode

import Ui.Native.Uid as Uid
import Ui.Container
import Ui.Pager
import Ui.Icons
import Ui

import Storage.Local
import Storage.Error

import Types as Types exposing (..)
import Dashboard as Dashboard
import Settings as Settings
import Form as Form

{-| Messages for the app.
-}
type Msg
  = Loaded (Result Storage.Error.Error (Maybe String))
  | Saved (Result Storage.Error.Error ())
  | Dashboard Dashboard.Msg
  | Settings Settings.Msg
  | Pager Ui.Pager.Msg
  | SelectPage Int
  | Form Form.Msg
  | Error String
  | SaveStore
  | Save


{-| Representation of a money tracker application.
-}
type alias Model =
  { dashboard : Dashboard.Model
  , settings : Settings.Model
  , pager : Ui.Pager.Model
  , form : Form.Model
  , store : Store
  }


{-| Initializes a money tracker.
-}
init : ( Model, Cmd Msg )
init =
  let
    initialCategories =
      [ { id = "0", name = "Transportation", icon = "android-bus"  }
      , { id = "1", name = "Food",           icon = "android-cart" }
      , { id = "2", name = "Bills",          icon = "cash"         }
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
      { dashboard = Dashboard.init
      , settings = Settings.init
      , pager = Ui.Pager.init ()
      , form = Form.init
      , store =
          { settings = { prefix = "", affix = "" }
          , categories = initialCategories
          , accounts = initialAccounts
          , transactions = []
          }
      }
  in
    ( model, Task.attempt Loaded (Storage.Local.get "moneytrack-data") )


{-| Updates a money tracker.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg_ model =
  case msg_ of
    Dashboard msg ->
      ( { model | dashboard = Dashboard.update msg model.dashboard }
      , Cmd.none )

    Pager msg ->
      ( { model | pager = Ui.Pager.update msg model.pager }
      , Cmd.none )

    Form msg ->
      let
        ( form, cmd ) =
          Form.update msg model.form
      in
        ( { model | form = form }, Cmd.map Form cmd )

    Settings msg ->
      let
        ( settings, cmd ) =
          Settings.update msg model.settings
      in
        ( updateSettings { model | settings = settings }
        , Cmd.map Settings cmd
        )

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
          { categoryId = data.categoryId
          , accountId = data.accountId
          , comment = data.comment
          , amount = data.amount
          , date = data.date
          , id = Uid.uid ()
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

    Loaded result ->
      case result of
        Ok maybeData ->
          case maybeData of
            Just data ->
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
                populateSettings { model | store = store }

            Nothing ->
              ( model, Cmd.none )

        Err _ ->
          ( model, Cmd.none )

    Saved _ ->
      let
        _ =
          log "Info" "Store saved in local storage."
      in
        ( model, Cmd.none )

    SaveStore ->
      let
        task =
          Storage.Local.set
            "moneytrack-data"
            (Json.Encode.encode 0 (encodeStore model.store))
      in
        ( model, Task.attempt Saved task )

    Error message ->
      let
        _ =
          log "Error" message
      in
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
          div [ onMouseDown (SelectPage 0) ] [ Ui.Icons.close [] ]

        bottomRight =
          div [ onMouseDown Save ] [ Ui.Icons.checkmark [] ]

        viewModel =
          { bottomRight = bottomRight
          , bottomLeft = bottomLeft
          , backMsg = SelectPage 0
          }
      in
        Form.view Form viewModel model.form
  in
    div []
      [ node "link" [ rel "stylesheet", href "style.css" ] []
      , Ui.Pager.view
          { pages =
            [ dashboard
            , form
            , settings
            ]
          , address = Pager
          }
          model.pager
      ]


{-| Populates the form with the given parameters.
-}
populateForm : Int -> Date.Date -> Model -> Model
populateForm amount date model =
  { model | form = Form.populate model.store amount date model.form }


{-| Populates the settings component from the store.
-}
populateSettings : Model -> ( Model, Cmd Msg )
populateSettings model =
  let
    ( settings, cmd ) =
      Settings.populate model.store.settings model.settings
  in
    ( { model | settings = settings }, Cmd.map Settings cmd )


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


{-| Subscriptions for the app.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Sub.map Form (Form.subscriptions model.form) ]


main : Program Never Model Msg
main =
  Html.program
    { subscriptions = subscriptions
    , update = update
    , view = view
    , init = init
    }
