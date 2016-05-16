module Rest exposing (..)

import Json.Decode as Json
import Json.Encode as J

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

promoteError : Http.RawError -> String
promoteError error =
  case error of
    Http.RawTimeout -> "Request timed out!"
    Http.RawNetworkError -> "Network error!"

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

request : String
        -> String
        -> List ( String, J.Value )
        -> Json.Decoder a
        -> (String -> msg)
        -> (a -> msg)
        -> Cmd msg
request url method params decoder error action =
  rawRequest method url (Http.string (encodeParams params)) decoder
    |> Task.perform error action

getRequest : String
           -> List (String, String)
           -> Json.Decoder a
           -> (String -> msg)
           -> (a -> msg)
           -> Cmd msg
getRequest url params decoder error action =
  rawRequest "GET" (Http.url url params) (Http.string "") decoder
    |> Task.perform error action

encodeParams : List ( String, J.Value ) -> String
encodeParams params =
  J.encode 0 (J.object params)

get url params decoder error action =
  getRequest url params decoder error action

post url params decoder error action =
  request url "POST" params decoder error action

patch url params decoder error action =
  request url "PATCH" params decoder error action

delete url params decoder error action =
  request url "DELETE" params decoder error action
