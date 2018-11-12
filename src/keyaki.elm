port module Main exposing (main)

import Browser
import Hepburn exposing (kana)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

-- ---------------------------
-- FIXME
-- ---------------------------

-- TODO in elm-webpack-starter.git /navigation branch for a `Routing` example
-- TODO Browser.document in nodefornerds.com/understanding-the-browser-document-elm-application-pt-1/

-- ---------------------------
-- PORTS
-- ---------------------------

port toJs : String -> Cmd msg

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
    Browser.sandbox
        { init = init
        ,update = update
        , view = view

        }


type alias Model =
    { content : String }


init : Model
init =
    { content = "" }

type Msg
    = Translate String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Translate newContent ->
            {
            model
             |
              content = newContent
            }


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Rōmaji to Kana", value model.content, onInput Translate ] []
        , div [] [ text (kana model.content) ]
        ]
