module Components.Folder exposing (..)

import Update.Extra.Infix exposing ((:>))
import Task

import Html exposing (node, text, div, b, span)
import Html.Events exposing (onClick)
import Html.App

import Components.Item as Item
import Types exposing (FolderContents, Video, Folder)

import Ui.Helpers.Emitter as Emitter
import Ui.DropdownMenu
import Ui.Container
import Ui.Loader
import Ui

type alias Model =
  { folders: List Item.Model
  , videos : List Item.Model
  , loader : Ui.Loader.Model
  , breadcrumbs : List Folder
  , current : Int
  , currentName : String
  , fabMenu: Ui.DropdownMenu.Model
  }

type Msg
  = Loader Ui.Loader.Msg
  | FabMenu Ui.DropdownMenu.Msg
  | Video Int Item.Msg
  | Folder Int Item.Msg
  | Error String
  | Loaded FolderContents
  | FinishLoading
  | CreateVideo
  | CreateFolder
  | Open Int
  | Refresh
  | Load Int

init : Model
init =
  let
    fabMenu = Ui.DropdownMenu.init
  in
    { folders = []
    , videos = []
    , breadcrumbs = []
    , loader = Ui.Loader.init 100
    , current = 0
    , currentName = ""
    , fabMenu = { fabMenu | offsetTop = 10
                          , favoredSides = { horizontal = "left"
                                           , vertical = "top"
                                           }
                }
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    basicSubs =
      [ Emitter.listenNaked "refresh" Refresh
      , Emitter.listenInt "open-folder" Open
      , Sub.map FabMenu (Ui.DropdownMenu.subscriptions model.fabMenu)
      ]

    folderSubs =
      List.map (\item -> Sub.map (Folder item.id) (Item.subscriptions item)) model.folders

    videoSubs =
      List.map (\item -> Sub.map (Video item.id) (Item.subscriptions item)) model.videos
  in
    Sub.batch (basicSubs ++ folderSubs ++ videoSubs)

initialize : Model -> Cmd Msg
initialize model =
  Task.perform (\_ -> Debug.crash "Never gonna happen") (\_ -> Load model.current) (Task.succeed ())

setData : FolderContents -> Model -> Model
setData contents model =
  { model | videos = List.map (Item.init "video") contents.videos
          , folders = List.map (Item.init "folder") contents.folders
          , breadcrumbs = contents.breadcrumbs
          , currentName = contents.name
          , current = contents.id
  }

update: Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Error message ->
      (model, Emitter.sendString "errors" message)
        :> update FinishLoading

    CreateFolder ->
      ( { model | fabMenu = Ui.DropdownMenu.close model.fabMenu }
      , Emitter.sendInt "create-folder" model.current)

    CreateVideo ->
      ( { model | fabMenu = Ui.DropdownMenu.close model.fabMenu }
      , Emitter.sendInt "create-video" model.current)

    Open id ->
      (model, Cmd.none)
       :> update (Load id)

    Refresh ->
      (model, Cmd.none)
       :> update (Load model.current)

    Loaded contents ->
      (setData contents model, Cmd.none)
        :> update FinishLoading

    FinishLoading ->
      ({ model | loader = Ui.Loader.finish model.loader }, Cmd.none)

    Load id ->
      let
        restCmd = Types.fetchFolderContents id Error Loaded
        (loader, cmd) = Ui.Loader.start model.loader
      in
        ({ model | loader = loader }, Cmd.batch [restCmd, Cmd.map Loader cmd])

    Video id act ->
      let
        (videos, commands) = listUpdate id act Video model.videos
      in
        ({ model | videos = videos }, commands)

    Folder id act ->
      let
        (folders, commands) = listUpdate id act Folder model.folders
      in
        ({ model | folders = folders }, commands)

    FabMenu act ->
      let
        fabMenu = Ui.DropdownMenu.update act model.fabMenu
      in
        ({ model | fabMenu = fabMenu }, Cmd.none)

    Loader act ->
      let
        (loader, cmd) = Ui.Loader.update act model.loader
      in
        ({ model | loader = loader }, Cmd.none)

emptyView : Html.Html msg
emptyView =
  node "video-library-folder-empty" []
    [ div []
      [ div []
        [ text "There are no "
        , b [] [text "folders"]
        ]
      , div []
        [ text "or "
        , b [] [text "videos"]
        , text " here yet!"
        ]
      ]
    , Ui.icon "ios-film" False []
    , div []
      [ div [] [text "Add one by clicking on the "
               , Ui.icon "plus-circled" False []
               , span [] [text " icon"]
               ]
      , div [] [text "at the bottom right corner."]
      ]
    ]

listUpdate : Int -> Item.Msg -> (Int -> Item.Msg -> Msg) -> List Item.Model -> (List Item.Model, Cmd Msg)
listUpdate id act tagger items =
  let
    updateItem item =
      if item.id == id then
        let
          (updatedItem, cmd) = Item.update act item
        in
          (updatedItem, Cmd.map (tagger id) cmd)
      else
        (item, Cmd.none)

    updatedList = List.map updateItem items
  in
    (List.map fst updatedList, Cmd.batch (List.map snd updatedList))

view : Model -> Html.Html Msg
view model =
  let
    breadcrumbItems =
      let
        root =
          [("Library", Just (Open 0))]

        base =
          if model.current /= 0 then
            root
          else
            []

        current =
          if model.current /= 0 then
            [(model.currentName, Just (Open model.current))]
          else
            root

        parts =
          List.filter (\folder -> folder.id /= 0) model.breadcrumbs
          |> List.map (\folder-> (folder.name, Just (Open folder.id)))
          |> List.reverse
      in
        base ++ parts ++ current

    folders =
      renderItems Folder model.folders

    videos =
      renderItems Video model.videos

    contents =
      if List.isEmpty (model.folders ++ model.videos) then
        emptyView
      else
        Ui.scrolledPanel
          [ node "video-library-folder" [] (folders ++ videos)
          ]
  in
    Ui.Container.view { align = "stretch"
                      , direction = "column"
                      , compact = True } []
      [ Ui.breadcrumbs (node "span" [] [text "/"]) breadcrumbItems
      , node "video-library-folder-wrapper" []
        [ Ui.Loader.barView model.loader
        , contents
        ]
      , Ui.DropdownMenu.view
        { element = Ui.fab "plus" []
        , items = [ Item.menuItem "android-film" "Add Video" CreateVideo
                  , Item.menuItem "folder" "Add Folder" CreateFolder
                  ]
        }
        FabMenu
        model.fabMenu
      ]

renderItems : (Int -> Item.Msg -> Msg) -> List Item.Model -> List (Html.Html Msg)
renderItems action items =
  List.map (renderItem action) items


renderItem : (Int -> Item.Msg -> Msg) -> Item.Model -> Html.Html Msg
renderItem action model =
  Html.App.map (action model.id) (Item.view model)
