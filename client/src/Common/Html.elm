module Common.Html exposing (anchorLink)

import App.Route exposing (Route, linkName)
import App.Types exposing (Msg)
import Html exposing (..)
import Html.Attributes exposing (..)


addSelectedClassIf : Bool -> String
addSelectedClassIf isSelected =
    case isSelected of
        True ->
            "selected"

        False ->
            ""


anchorLink : Maybe Route -> String -> Html Msg
anchorLink currentRoute endpoint =
    a [ class (addSelectedClassIf (endpoint == linkName currentRoute)), href ("/" ++ String.toLower endpoint) ]
        [ text endpoint ]
