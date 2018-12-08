port module Main exposing (Document, Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Hepburn exposing (kana)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- ---------------------------
-- FIXME
-- ---------------------------
-- TODO in elm-webpack-starter.git /navigation branch for a `Routing` example
-- TODO Browser.document in nodefornerds.com/understanding-the-browser-document-elm-application-pt-1/
-- ---------------------------
-- PORTS
-- ---------------------------
--port toJs : String -> Cmd msg
-- ---------------------------
-- MAIN
-- ---------------------------
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


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { content : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { content = "" }, Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Translate String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Translate newContent ->
            ( { model
                | content = newContent
              }
            , Cmd.none
            )



-- ---------------------------
-- SUBSCRIPTIONS
-- github.com/jakewitcher/Elm_Browser-dot-document-Boilerplate
-- ---------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- ---------------------------
-- VIEW
-- ---------------------------


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "Hello Goodbye"
    , body =
        [ div []
            [ input [ placeholder "Rōmaji to Kana", value model.content, onInput Translate ] []
            , div [] [ text (kana model.content) ]
            ]
        ]
    }
