module Ui.SearchInput where

import Ext.Signal exposing ((>>>))
import Html exposing (node)
import Time exposing (Time)
import Ext.Date
import Effects
import Task
import Date

import Ui.Input
import Ui

import Debug exposing (log)

type alias Model =
  { mailbox : Signal.Mailbox String
  , input : Ui.Input.Model
  , timestamp : Time
  , timeout : Time
  }

type Action
  = Input Ui.Input.Action
  | Update Time
  | Tasks ()

init : Time -> Model
init timeout =
  let
    input = Ui.Input.init ""
  in
    { input = { input | placeholder = "Search" }
    , mailbox = Signal.mailbox ""
    , timeout = timeout
    , timestamp = 0
    }

update: Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Update time ->
      let
        effect =
          if time == (model.timestamp + model.timeout) then
            Signal.send model.mailbox.address (model.input.value)
            |> Effects.task
            |> Effects.map Tasks
          else
            Effects.none
      in
        (model, effect)
    Input act ->
      let
        justNow = now Nothing

        updatedModel =
          { model | input = Ui.Input.update act model.input
                  , timestamp = justNow }
        effect =
          Task.andThen
            (Task.sleep model.timeout)
            (\_ -> Task.succeed (Update (justNow + model.timeout)))
            |> Effects.task
      in
        (updatedModel, effect)
    Tasks _ -> (model, Effects.none)

view: Signal.Address Action -> Model -> Html.Html
view address model =
  node "ui-search-input" []
    [ Ui.icon "search" False []
    , Ui.Input.view (address >>> Input) model.input
    ]

now : a -> Time
now a =
  Date.toTime (Ext.Date.now a)
