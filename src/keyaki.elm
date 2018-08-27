module Main exposing (main)

import Browser
import Hepburn exposing (kana)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Utils


main =
    Utils.sandbox init update view


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
            { model | content = newContent }


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Rōmaji to Kana", value model.content, onInput Translate ] []
        , div [] [ text (kana model.content) ]
        ]
