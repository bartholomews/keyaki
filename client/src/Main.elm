port module Main exposing (main, toJs, update)

import App.State as App exposing (..)
import App.Types as App exposing (Model, Msg)
import App.View as App exposing (..)
import Browser



-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    App.update



-- https://guide.elm-lang.org/webapps/navigation.html


main : Program Int Model Msg
main =
    Browser.application
        { init = \_ url key -> ( App.initialModel url key, App.initialCommand )
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
