module Common.Html exposing (anchorLink)

import App.Route as Route exposing (Route, stringifyLinkName)
import App.Types exposing (Msg, RouteLink)
import Html exposing (..)
import Html.Attributes exposing (..)


anchorLink : Maybe Route -> RouteLink -> Html Msg
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
        [ text (stringifyLinkName routeLink.linkName) ]
