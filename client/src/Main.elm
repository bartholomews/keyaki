port module Main exposing (main, toJs, update)

import App.State as App exposing (..)
import App.Types as App exposing (Config, Model, Msg)
import App.View as App exposing (..)
import Browser
import Url exposing (Protocol(..))



-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    App.update



-- https://guide.elm-lang.org/webapps/navigation.html


getConfig : Config
getConfig =
    { serverApi =
        { protocol = Http
        , host = "localhost"
        , port_ = Just 8081
        , path = "/api"
        , query = Nothing
        , fragment = Nothing
        }
    }


main : Program Int Model Msg
main =
    Browser.application
        { init = \_ url key -> ( App.initialModel url key getConfig, App.initialCommand )
        , view =
            \m ->
                { title = "ケヤキ"
                , body = [ App.root m ]
                }
        , update = App.update
        , subscriptions = always Sub.none
        , onUrlChange = App.UrlChanged
        , onUrlRequest = App.LinkClicked
        }



-- ---------------------------
-- SUBSCRIPTIONS
-- github.com/jakewitcher/Elm_Browser-dot-document-Boilerplate
-- ---------------------------
