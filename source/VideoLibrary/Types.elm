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
  { folderId : Int
  , image : String
  , name : String
  , url : String
  , id : Int
  }

type alias Folder =
  { folderId : Int
  , image : String
  , name : String
  , id : Int
  }

videoDecoder : Json.Decoder Video
videoDecoder =
  Json.succeed Video
    |: ("folderId" := Json.int)
    |: ("image" := Json.string)
    |: ("name" := Json.string)
    |: ("url" := Json.string)
    |: ("id" := Json.int)

folderDecoder : Json.Decoder Folder
folderDecoder =
  Json.succeed Folder
    |: ("folderId" := Json.int)
    |: ("image" := Json.string)
    |: ("name" := Json.string)
    |: ("id" := Json.int)

-- Queries
fetchVideos : Int -> (Result String (List Video) -> a) -> Effects.Effects a
fetchVideos folderId action =
  Rest.get
    "http://localhost:8002/videos/"
    [("folderId", (toString folderId))]
    (Json.list videoDecoder)
    action

fetchFolders : Int -> (Result String (List Folder) -> a) -> Effects.Effects a
fetchFolders folderId action =
  Rest.get
    "http://localhost:8002/folders/"
    [("folderId", (toString folderId))]
    (Json.list folderDecoder)
    action

fetchVideo : Int -> (Result String Video -> a) -> Effects.Effects a
fetchVideo id action =
  Rest.get
    ("http://localhost:8002/videos/" ++ (toString id))
    []
    videoDecoder
    action

createVideo : List (String, J.Value) -> (Result String Video -> a) -> Effects.Effects a
createVideo params action =
  Rest.post
    "http://localhost:8002/videos/"
    params
    videoDecoder
    action

patchVideo : Int -> List (String, J.Value) -> (Result String Video -> a) -> Effects.Effects a
patchVideo id params action =
  Rest.patch
    ("http://localhost:8002/videos/" ++ (toString id))
    params
    videoDecoder
    action

createFolder: List (String, J.Value) -> (Result String Folder -> a) -> Effects.Effects a
createFolder params action =
  Rest.post
    "http://localhost:8002/folders/"
    params
    folderDecoder
    action

patchFolder : Int -> List (String, J.Value) -> (Result String Folder -> a) -> Effects.Effects a
patchFolder id params action =
  Rest.patch
    ("http://localhost:8002/folders/" ++ (toString id))
    params
    folderDecoder
    action
