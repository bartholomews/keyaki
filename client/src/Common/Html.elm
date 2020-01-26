module Common.Html exposing (anchorLink)

import App.Route as Route exposing (Route, stringifyLinkName)
import App.Types exposing (HeaderLink, Msg)
import Html exposing (..)
import Html.Attributes exposing (..)


anchorLink : Maybe Route -> HeaderLink -> Html Msg
anchorLink maybeRoute routeLink =
    let
        isSelected =
            Route.matches maybeRoute routeLink
    in
    a
        [ class
            (if isSelected then
                "selected"

             else
                ""
            )
        , href ("/" ++ String.toLower routeLink.urlPath)
        ]
        [ text (stringifyLinkName routeLink.name) ]
