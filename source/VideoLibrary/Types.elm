module VideoLibrary.Types where

import Json.Decode.Extra exposing ((|:))
import Json.Decode as Json exposing ((:=))
import Json.Encode as J

import Maybe.Extra exposing (isJust)
import Effects
import Task
import Http

import VideoLibrary.Rest as Rest

import Debug exposing (log)

type alias Video =
  { folderId : String
  , image : String
  , name : String
  , url : String
  , id : String
  }

type alias Folder =
  { folderId : String
  , image : String
  , name : String
  , id : String
  }

videoDecoder : Json.Decoder Video
videoDecoder =
  Json.succeed Video
    |: ("folderId" := Json.string)
    |: ("image" := Json.string)
    |: ("name" := Json.string)
    |: ("url" := Json.string)
    |: ("id" := Json.string)

folderDecoder : Json.Decoder Folder
folderDecoder =
  Json.succeed Folder
    |: ("folderId" := Json.string)
    |: ("image" := Json.string)
    |: ("name" := Json.string)
    |: ("id" := Json.string)

-- Queries
fetchVideos : String -> (Result String (List Video) -> a) -> Effects.Effects a
fetchVideos folderId action =
  Rest.get
    "http://localhost:8002/videos/"
    [("folderId", folderId)]
    (Json.list videoDecoder)
    action

fetchFolders : String -> (Result String (List Folder) -> a) -> Effects.Effects a
fetchFolders folderId action =
  Rest.get
    "http://localhost:8002/folders/"
    [("folderId", folderId)]
    (Json.list folderDecoder)
    action

fetchVideo : String -> (Result String Video -> a) -> Effects.Effects a
fetchVideo id action =
  Rest.get
    ("http://localhost:8002/videos/" ++ id)
    []
    videoDecoder
    action
