module Types exposing (..)

import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode as Json
import Json.Encode as J

import Maybe.Extra exposing (isJust)
import Task
import Http

import Ui.Helpers.Env as Env
import Rest as Rest

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
  Env.get "endpoint" Json.string
    |> Result.withDefault "http://localhost:8002/"

videoDecoder : Json.Decoder Video
videoDecoder =
  decode Video
    |> required "folder_id" Json.int
    |> required "image" Json.string
    |> required "name" Json.string
    |> required "url" Json.string
    |> required "id" Json.int

folderDecoder : Json.Decoder Folder
folderDecoder =
  decode Folder
    |> required "folder_id" Json.int
    |> required "image" Json.string
    |> required "name" Json.string
    |> required "id" Json.int

searchDataDecoder : Json.Decoder SearchData
searchDataDecoder =
  decode SearchData
    |> required "folders" (Json.list folderDecoder)
    |> required "videos" (Json.list videoDecoder)

folderContentsDecoder : Json.Decoder FolderContents
folderContentsDecoder =
  decode FolderContents
    |> required "breadcrumbs" (Json.list folderDecoder)
    |> required "folders"( Json.list folderDecoder)
    |> required "videos" (Json.list videoDecoder)
    |> required "folder_id" Json.int
    |> required "name" Json.string
    |> required "id" Json.int

fetchFolderContents : Int -> (String -> msg) -> (FolderContents -> msg) -> Cmd msg
fetchFolderContents folderId error action =
  Rest.get
    (endpoint ++ "folders/" ++ (toString folderId) ++ "/contents")
    []
    folderContentsDecoder
    error
    action

fetchVideo : Int -> (String -> msg) -> (Video -> msg) -> Cmd msg
fetchVideo id error action =
  Rest.get
    (endpoint ++ "videos/" ++ (toString id))
    []
    videoDecoder
    error
    action

fetchFolder : Int -> (String -> msg) -> (Folder -> msg) -> Cmd msg
fetchFolder id error action =
  Rest.get
    (endpoint ++ "folders/" ++ (toString id))
    []
    folderDecoder
    error
    action

createVideo : List (String, J.Value) -> (String -> msg ) -> (Video -> msg) -> Cmd msg
createVideo params error action =
  Rest.post
    (endpoint ++ "videos/")
    params
    videoDecoder
    error
    action

deleteVideo : Int -> (String -> msg) -> (J.Value -> msg) -> Cmd msg
deleteVideo id error action =
  Rest.delete
    (endpoint ++ "videos/" ++ (toString id))
    []
    Json.value
    error
    action

patchVideo : Int -> List (String, J.Value) -> (String -> msg) -> (Video -> msg) -> Cmd msg
patchVideo id params error action =
  Rest.patch
    (endpoint ++ "videos/" ++ (toString id))
    params
    videoDecoder
    error
    action

createFolder: List (String, J.Value) -> (String -> msg) -> (Folder -> msg) -> Cmd msg
createFolder params error action =
  Rest.post
    (endpoint ++ "folders/")
    params
    folderDecoder
    error
    action

deleteFolder : Int -> (String -> msg) -> (Json.Value -> msg) -> Cmd msg
deleteFolder id error action =
  Rest.delete
    (endpoint ++ "folders/" ++ (toString id))
    []
    Json.value
    error
    action

patchFolder : Int -> List (String, J.Value) -> (String -> msg) -> (Folder -> msg) -> Cmd msg
patchFolder id params error action =
  Rest.patch
    (endpoint ++ "folders/" ++ (toString id))
    params
    folderDecoder
    error
    action

search : String -> (String -> msg) -> (SearchData -> msg) -> Cmd msg
search query error action =
  Rest.get
    (endpoint ++ "video-library/search")
    [("query", query)]
    searchDataDecoder
    error
    action
