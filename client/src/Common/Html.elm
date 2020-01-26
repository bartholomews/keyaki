module Common.Html exposing (..)

import App.Types exposing (Msg)
import Html exposing (..)
import Html.Attributes exposing (..)


anchorLink : String -> Maybe String -> Html Msg
anchorLink endpoint textStr =
    a [ href ("/" ++ endpoint) ] [ text (Maybe.withDefault endpoint textStr) ]
