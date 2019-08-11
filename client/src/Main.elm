port module Main exposing (main, toJs, update)

--(httpErrorToString, init, view)

import App.State as App exposing (..)
import App.Types as App exposing (Model, Msg)
import App.View as App exposing (..)
import Browser



--import Html exposing (..)
--import Http exposing (Error(..))
-- ---------------------------
-- TODO in elm-webpack-starter.git /navigation branch for a `Routing` example
-- TODO Browser.document in nodefornerds.com/understanding-the-browser-document-elm-application-pt-1/
-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------
--type alias Model =
--    { counter : Int
--    , serverMessage : String
--    }
--type alias Model =
--    { todos : App.Todos.Todos
--    , todosVisibility : Todos.Visibility
--    , newTodo : Todo.Todo
--    }
-- Model
--init : Int -> ( Model, Cmd Msg )
--init flags =
--    ( App.initialModel, App.initialCommand )
-- ---------------------------
-- UPDATE
-- ---------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    App.update



--httpErrorToString : Http.Error -> String
--httpErrorToString err =
--    case err of
--        BadUrl _ ->
--            "BadUrl"
--
--        Timeout ->
--            "Timeout"
--
--        NetworkError ->
--            "NetworkError"
--
--        BadStatus _ ->
--            "BadStatus"
--
--        BadBody _ ->
--            "BadPayload"
-- ---------------------------
-- VIEW
-- ---------------------------
--view : Model -> Html Msg
--view =
--    App.root
-- ---------------------------
-- MAIN
-- ---------------------------
--
--main : Program Int Model Msg
--main =
--    Browser.document
--        { init = init
--        , update = update
--        , view =
--            \m ->
--                { title = "Elm 0.19 starter"
--                , body = [ view m ]
--                }
--        , subscriptions = \_ -> Sub.none
--        }


main : Program Int Model Msg
main =
    Browser.document
        { init = \_ -> ( App.initialModel, App.initialCommand )
        , view =
            \m ->
                { title = "ケヤキ"
                , body = [ App.root m ]
                }
        , update = App.update
        , subscriptions = always Sub.none
        }



-- ---------------------------
-- SUBSCRIPTIONS
-- github.com/jakewitcher/Elm_Browser-dot-document-Boilerplate
-- ---------------------------
