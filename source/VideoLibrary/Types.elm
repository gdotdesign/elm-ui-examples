module VideoLibrary.Types where

import Json.Decode.Extra exposing ((|:))
import Json.Decode as Json exposing ((:=))

import Maybe.Extra exposing (isJust)
import Effects
import Task
import Http

import Debug exposing (log)

type Item
  = FolderNode Folder
  | VideoNode Video

type alias Video =
  { url : String
  , image : String
  , name : String
  , id : String
  }

type alias Folder =
  { name : String
  , items : List Item
  , id : String
  }

-- Decoders
-- Lazy decode with a decoder for recursive structures...
lazy : (() -> Json.Decoder a) -> Json.Decoder a
lazy thunk =
  Json.customDecoder Json.value
    (\js -> Json.decodeValue (thunk ()) js)

itemDecoder : Json.Decoder Item
itemDecoder =
  Json.oneOf
    [ Json.map VideoNode videoDecoder
    , Json.map FolderNode (lazy (\_ -> folderDecoder))
    ]

videoDecoder : Json.Decoder Video
videoDecoder =
  Json.succeed Video
    |: ("url" := Json.string)
    |: ("image" := Json.string)
    |: ("name" := Json.string)
    |: ("id" := Json.string)

folderDecoder : Json.Decoder Folder
folderDecoder =
  Json.succeed Folder
    |: ("name" := Json.string)
    |: ("items" := Json.list itemDecoder)
    |: ("id" := Json.string)

-- Functions
itemName : Item -> String
itemName item =
  case item of
    VideoNode video -> video.name
    FolderNode folder -> folder.name

itemPath : String -> List (Folder) -> Folder -> List (Folder)
itemPath id path folder =
  let
    inFolder = hasItem id folder
    inSubFolder = hasItemRecursive id folder
  in
    if inFolder then
      path ++ [folder]
    else if inSubFolder then
      case folderOf id folder of
        Just subfolder -> itemPath id (path ++ [folder]) subfolder
        Nothing -> path
    else
      path

folderOf : String -> Folder -> Maybe Folder
folderOf id folder =
  List.map asFolder folder.items
    |> List.filter (\mf -> Maybe.withDefault False (Maybe.map (\item -> hasItemRecursive id item) mf))
    |> List.head
    |> Maybe.Extra.join

hasItemRecursive : String -> Folder -> Bool
hasItemRecursive id folder =
  flatten (FolderNode folder)
    |> List.map (\item -> testId id item)
    |> List.member True

hasItem : String -> Folder -> Bool
hasItem id folder =
  List.map (\item -> testId id item) folder.items
    |> List.member True

findItemByID :  String -> List Item -> Maybe Item
findItemByID id items =
  List.map flatten items
    |> List.foldl (++) []
    |> List.filter (\item -> testId id item)
    |> List.head

testId : String -> Item -> Bool
testId id item =
  case item of
    FolderNode folder -> folder.id == id
    VideoNode video -> video.id == id

flatten : Item -> List Item
flatten item =
  case item of
    FolderNode folder ->
      [FolderNode folder] ++ (List.map flatten folder.items
                               |> List.foldl (++) [])
    VideoNode video -> [VideoNode video]

firstFolder : List Item -> Maybe Folder
firstFolder items =
  List.filter isFolder items
    |> List.head
    |> maybeAsFolder

maybeAsFolder : Maybe Item -> Maybe Folder
maybeAsFolder item =
  Maybe.map asFolder item
    |> Maybe.Extra.join

maybeAsVideo : Maybe Item -> Maybe Video
maybeAsVideo item =
  Maybe.map asVideo item
    |> Maybe.Extra.join

asFolder : Item -> Maybe Folder
asFolder item =
  case item of
    FolderNode folder -> Just folder
    VideoNode video -> Nothing

asVideo : Item -> Maybe Video
asVideo item =
  case item of
    FolderNode folder -> Nothing
    VideoNode video -> Just video

isFolder : Item -> Bool
isFolder item =
  case item of
    VideoNode video -> False
    FolderNode folder -> True

itemImage : Item -> String
itemImage item =
  case item of
    VideoNode video -> video.image
    FolderNode folder -> folderImage folder

folderImage : Folder -> String
folderImage folder =
  Maybe.map itemImage (List.head folder.items)
    |> Maybe.withDefault ""

-- Queries
fetchData : (Maybe (List Item) -> a) -> Effects.Effects a
fetchData action =
  Http.get (Json.list itemDecoder) "/video-library-data.json"
    |> Task.toMaybe
    |> Task.map action
    |> Effects.task
