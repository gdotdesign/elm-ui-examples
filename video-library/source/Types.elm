module Types where

import Json.Decode.Extra exposing ((|:))
import Json.Decode as Json exposing ((:=))
import Json.Encode as J

import Maybe.Extra exposing (isJust)
import Effects
import Task
import Http

import Rest as Rest
import Utils.Env as Env

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

type alias FolderContents =
  { breadcrumbs: List Folder
  , folders : List Folder
  , videos : List Video
  , folderId : Int
  , name : String
  , id : Int
  }

type alias SearchData =
  { folders : List Folder
  , videos : List Video
  }

endpoint : String
endpoint =
  Env.get "endpoint" "http://localhost:8002/" Json.string

videoDecoder : Json.Decoder Video
videoDecoder =
  Json.succeed Video
    |: ("folder_id" := Json.int)
    |: ("image" := Json.string)
    |: ("name" := Json.string)
    |: ("url" := Json.string)
    |: ("id" := Json.int)

folderDecoder : Json.Decoder Folder
folderDecoder =
  Json.succeed Folder
    |: ("folder_id" := Json.int)
    |: ("image" := Json.string)
    |: ("name" := Json.string)
    |: ("id" := Json.int)

searchDataDecoder : Json.Decoder SearchData
searchDataDecoder =
  Json.succeed SearchData
    |: ("folders" := Json.list folderDecoder)
    |: ("videos" := Json.list videoDecoder)

folderContentsDecoder : Json.Decoder FolderContents
folderContentsDecoder =
  Json.succeed FolderContents
    |: ("breadcrumbs" := Json.list folderDecoder)
    |: ("folders" := Json.list folderDecoder)
    |: ("videos" := Json.list videoDecoder)
    |: ("folder_id" := Json.int)
    |: ("name" := Json.string)
    |: ("id" := Json.int)

fetchFolderContents : Int -> (Result String FolderContents -> a) -> Effects.Effects a
fetchFolderContents folderId action =
  Rest.get
    (endpoint ++ "folders/" ++ (toString folderId) ++ "/contents")
    []
    folderContentsDecoder
    action

fetchVideo : Int -> (Result String Video -> a) -> Effects.Effects a
fetchVideo id action =
  Rest.get
    (endpoint ++ "videos/" ++ (toString id))
    []
    videoDecoder
    action

createVideo : List (String, J.Value) -> (Result String Video -> a) -> Effects.Effects a
createVideo params action =
  Rest.post
    (endpoint ++ "videos/")
    params
    videoDecoder
    action

deleteVideo : Int -> (Result String J.Value -> a) -> Effects.Effects a
deleteVideo id action =
  Rest.delete
    (endpoint ++ "videos/" ++ (toString id))
    []
    Json.value
    action

patchVideo : Int -> List (String, J.Value) -> (Result String Video -> a) -> Effects.Effects a
patchVideo id params action =
  Rest.patch
    (endpoint ++ "videos/" ++ (toString id))
    params
    videoDecoder
    action

createFolder: List (String, J.Value) -> (Result String Folder -> a) -> Effects.Effects a
createFolder params action =
  Rest.post
    (endpoint ++ "folders/")
    params
    folderDecoder
    action

deleteFolder : Int -> (Result String J.Value -> a) -> Effects.Effects a
deleteFolder id action =
  Rest.delete
    (endpoint ++ "folders/" ++ (toString id))
    []
    Json.value
    action

patchFolder : Int -> List (String, J.Value) -> (Result String Folder -> a) -> Effects.Effects a
patchFolder id params action =
  Rest.patch
    (endpoint ++ "folders/" ++ (toString id))
    params
    folderDecoder
    action

search : String -> (Result String SearchData -> a) -> Effects.Effects a
search query action =
  Rest.get
    (endpoint ++ "video-library/search")
    [("query", query)]
    searchDataDecoder
    action
