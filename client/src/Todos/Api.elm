module Todos.Api exposing (getTodos)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Todo.Types as Todo
import Todos.Types exposing (..)


getTodos : Cmd Msg
getTodos =
    Http.get
        { url = "http://localhost:8080/api/todos/"
        , expect = Http.expectJson TodosFetched todosDecoder
        }



-- https://github.com/NoRedInk/elm-json-decode-pipeline


todoDecoder : Decoder Todo.Todo
todoDecoder =
    Decode.succeed Todo.Todo
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "completed" Decode.bool
        |> Pipeline.required "description" Decode.string


todosDecoder : Decoder (List Todo.Todo)
todosDecoder =
    Decode.list todoDecoder
