module VideoLibrary.Rest where

import Json.Decode as Json
import Json.Encode as J

import Effects
import Task
import Http

send : String -> String -> Http.Body -> Task.Task Http.RawError Http.Response
send method url body =
  let
    settings =
      { timeout = 0
      , onStart = Nothing
      , onProgress = Nothing
      , desiredResponseType = Nothing
      , withCredentials = True
      }
    request =
      { headers = [("Content-Type", "application/json")]
      , verb = method
      , body = body
      , url = url
      }
  in
    Http.send settings request

rawRequest : String -> String -> Http.Body -> Json.Decoder a -> Task.Task String a
rawRequest method url body decoder =
  let
    response =
      send method url body
  in
    Task.andThen
      (Task.mapError promoteError response)
      (\response -> handleResponse decoder response)

handleResponse : Json.Decoder a -> Http.Response -> Task.Task String a
handleResponse decoder response =
  if response.status == 500 then
    handleApiError response
  else if 200 <= response.status && response.status < 300 then
    handleApiResponse decoder response
  else
    Task.fail "Invalid status."

handleApiError : Http.Response -> Task.Task String a
handleApiError response =
  case response.value of
    Http.Text str ->
      Task.fail ("Got invalid response: " ++ str)
    Http.Blob _ -> Task.fail "Wrong response type(Blob)!"

handleApiResponse : Json.Decoder a -> Http.Response -> Task.Task String a
handleApiResponse decoder response =
  case response.value of
    Http.Text str ->
      case Json.decodeString decoder str of
        Ok data -> Task.succeed data
        Err msg -> Task.fail "Invalid JSON data!"
    Http.Blob _ -> Task.fail "Wrong response type(Blob)!"

request url method params decoder action =
  rawRequest method url (Http.string (encodeParams params)) decoder
    |> Task.toResult
    |> Task.map action
    |> Effects.task

getRequest url params decoder action =
  rawRequest "GET" (Http.url url params) (Http.string "") decoder
    |> Task.toResult
    |> Task.map action
    |> Effects.task


promoteError : Http.RawError -> String
promoteError error =
  case error of
    Http.RawTimeout -> "Request timed out!"
    Http.RawNetworkError -> "Network error!"

encodeParams : List ( String, J.Value ) -> String
encodeParams params =
  J.encode 0 (J.object params)

get url params decoder action =
  getRequest url params decoder action

post url params decoder action =
  request url "POST" params decoder action

patch url params decoder action =
  request url "PATCH" params decoder action

delete url params decoder action =
  request url "DELETE" params decoder action
