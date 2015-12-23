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
  }

type alias Folder =
  { name : String
  , items : List Item
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
  Json.object2 Video
    ("url" := Json.string)
    ("image" := Json.string)

folderDecoder : Json.Decoder Folder
folderDecoder =
  Json.object2 Folder
    ("name" := Json.string)
    ("items" := Json.list itemDecoder)


-- Functions
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
