module App.Types exposing (LinkName(..), Model, Msg(..), RouteLink)

import Browser
import Browser.Navigation as Nav
import Todo.Types as Todo
import Todos.Types as Todos
import Url exposing (Url)


type alias Model =
    { url : Url
    , navKey : Nav.Key
    , todos : Todos.Todos
    , todosVisibility : Todos.Visibility
    , newTodo : Todo.Todo
    }


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | TodosMsg Todos.Msg
    | TodoMsg Todo.Msg
    | DoNothing


type LinkName
    = HOME
    | SRS
    | TODO


type alias RouteLink =
    { urlPath : String
    , linkName : LinkName
    }
