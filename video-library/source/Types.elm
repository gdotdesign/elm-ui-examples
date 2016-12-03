module Types exposing (..)

import Json.Decode.Pipeline exposing (decode, required)
import Json.Decode as Json
import Json.Encode as J

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

fetchFolderContents : Int -> (Result String FolderContents -> msg) -> Cmd msg
fetchFolderContents folderId msg =
  Rest.get
    (endpoint ++ "folders/" ++ (toString folderId) ++ "/contents")
    []
    folderContentsDecoder
    msg

fetchVideo : Int -> (Result String Video -> msg) -> Cmd msg
fetchVideo id msg =
  Rest.get
    (endpoint ++ "videos/" ++ (toString id))
    []
    videoDecoder
    msg

fetchFolder : Int -> (Result String Folder -> msg) -> Cmd msg
fetchFolder id msg =
  Rest.get
    (endpoint ++ "folders/" ++ (toString id))
    []
    folderDecoder
    msg

createVideo : List (String, J.Value) -> (Result String Video -> msg) -> Cmd msg
createVideo params msg =
  Rest.post
    (endpoint ++ "videos/")
    params
    videoDecoder
    msg

deleteVideo : Int -> (Result String J.Value -> msg) -> Cmd msg
deleteVideo id msg =
  Rest.delete
    (endpoint ++ "videos/" ++ (toString id))
    []
    Json.value
    msg

patchVideo : Int -> List (String, J.Value) -> (Result String Video -> msg) -> Cmd msg
patchVideo id params msg =
  Rest.patch
    (endpoint ++ "videos/" ++ (toString id))
    params
    videoDecoder
    msg

createFolder: List (String, J.Value) -> (Result String Folder -> msg) -> Cmd msg
createFolder params msg =
  Rest.post
    (endpoint ++ "folders/")
    params
    folderDecoder
    msg

deleteFolder : Int -> (Result String J.Value -> msg) -> Cmd msg
deleteFolder id msg =
  Rest.delete
    (endpoint ++ "folders/" ++ (toString id))
    []
    Json.value
    msg

patchFolder : Int -> List (String, J.Value) -> (Result String Folder -> msg) -> Cmd msg
patchFolder id params msg =
  Rest.patch
    (endpoint ++ "folders/" ++ (toString id))
    params
    folderDecoder
    msg

search : String -> (Result String SearchData -> msg) -> Cmd msg
search query msg =
  Rest.get
    (endpoint ++ "video-library/search")
    [("query", query)]
    searchDataDecoder
    msg
