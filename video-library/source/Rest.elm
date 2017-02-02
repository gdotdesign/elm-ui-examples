module Rest exposing (..)

import Json.Decode as Json
import Json.Encode as J

import Task
import Http

send : String -> String -> List (String, J.Value) -> Json.Decoder a -> (Result String a -> msg) -> Cmd msg
send url method body decoder msg =
  { headers = []
  , body = Http.jsonBody (J.object body)
  , expect = Http.expectJson decoder
  , withCredentials = True
  , timeout = Nothing
  , method = method
  , url = url
  }
    |> Http.request
    |> Http.toTask
    |> Task.mapError promoteError
    |> Task.attempt msg

promoteError : Http.Error -> String
promoteError error =
  case error of
    Http.Timeout -> "Request timed out!"
    Http.NetworkError -> "Network error!"
    _ -> "Unkown error!"

get url params decoder msg =
  send url "GET" [] decoder msg

post url params decoder msg =
  send url "POST" params decoder msg

patch url params decoder msg =
  send url "PATCH" params decoder msg

delete url params decoder msg=
  send url "DELETE" params decoder msg
