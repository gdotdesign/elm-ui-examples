module VideoLibrary.Types where

import Json.Decode.Extra exposing ((|:))
import Json.Decode as Json exposing ((:=))

import Maybe.Extra exposing (isJust)
import Effects
import Task
import Http

import Entities.Association as Association exposing (Association)

import Debug exposing (log)

type alias Video =
  { url : String
  , folderId : String
  , image : String
  , name : String
  , id : String
  }

type alias Folder =
  { name : String
  , folderId : String
  , id : String
  }

type alias Repository =
  { videos : List Video
  , folders : List Folder
  , videosAndFolders : Association Video Folder
  , foldersAndFolders : Association Folder Folder
  }

initRepository : List Video -> List Folder -> Repository
initRepository videos folders =
  { videos = videos
  , folders = folders
  , videosAndFolders = Association.init .folderId videos folders
  , foldersAndFolders = Association.init .folderId folders folders
  }

videoDecoder : Json.Decoder Video
videoDecoder =
  Json.succeed Video
    |: ("folderId" := Json.string)
    |: ("url" := Json.string)
    |: ("image" := Json.string)
    |: ("name" := Json.string)
    |: ("id" := Json.string)

folderDecoder : Json.Decoder Folder
folderDecoder =
  Json.succeed Folder
    |: ("folderId" := Json.string)
    |: ("name" := Json.string)
    |: ("id" := Json.string)

-- Queries
fetchVideos : (Maybe (List Video) -> a) -> Effects.Effects a
fetchVideos action =
  Http.get (Json.list videoDecoder) "http://localhost:8002/videos"
    |> Task.toMaybe
    |> Task.map action
    |> Effects.task

fetchFolders : (Maybe (List Folder) -> a) -> Effects.Effects a
fetchFolders action =
  Http.get (Json.list folderDecoder) "http://localhost:8002/folders"
    |> Task.toMaybe
    |> Task.map action
    |> Effects.task
