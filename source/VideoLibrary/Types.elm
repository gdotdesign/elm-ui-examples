module VideoLibrary.Types where

import Json.Decode.Extra exposing ((|:))
import Json.Decode as Json exposing ((:=))

import Effects
import Task
import Http

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

findFolderById :  String -> List Item -> Maybe Folder
findFolderById id items =
  List.map flatten items
    |> List.foldl (++) []
    |> List.filter (\item -> testId id item)
    |> List.head
    |> asFolder

testId : String -> Item -> Bool
testId id item =
  case item of
    FolderNode folder -> folder.id == id
    VideoNode video -> video.id == id

flatten : Item -> List Item
flatten item =
  case item of
    FolderNode folder -> [FolderNode folder] ++ (List.map flatten folder.items
                                      |> List.foldl (++) [])
    VideoNode video -> [VideoNode video]

firstFolder : List Item -> Maybe Folder
firstFolder items =
  List.filter isFolder items
    |> List.head
    |> asFolder

asFolder : Maybe Item -> Maybe Folder
asFolder item =
  case item of
    Nothing -> Nothing
    Just item ->
      case item of
        FolderNode folder -> Just folder
        VideoNode video -> Nothing

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

fetchData : (Maybe (List Item) -> a) -> Effects.Effects a
fetchData action =
  Http.get (Json.list itemDecoder) "/video-library-data.json"
    |> Task.toMaybe
    |> Task.map action
    |> Effects.task
