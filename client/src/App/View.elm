module App.View exposing (root)

import App.Route exposing (Route(..), route)
import App.Types exposing (LinkName(..), Model, Msg(..), RouteLink)
import Common.Html exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Home as Home
import Page.Srs as Srs
import Url.Parser exposing (parse)



-- https://stackoverflow.com/questions/52544763/how-to-create-spa-with-elm-0-19


root : Model -> Html Msg
root model =
    div []
        [ div [ class "header-content" ]
            [ header [ class "btn btn-primary" ]
                [ img [ src "/dist/images/favicon.ico", alt "ケヤキ" ] []
                , navigation (parse route model.url)
                ]
            ]
        , content model
        ]


navigation : Maybe Route -> Html Msg
navigation currentRoute =
    nav []
        [ ul []
            [ li [] [ anchorLink currentRoute { urlPath = "home", linkName = HOME } ]
            , li [] [ anchorLink currentRoute { urlPath = "srs?jlpt=1", linkName = SRS } ]
            ]
        ]


content : Model -> Html Msg
content model =
    main_ [ class "main-content" ]
        [ case parse route model.url of
            Just path ->
                matchedRoute model path

            Nothing ->
                Home.content model
        ]


matchedRoute : Model -> Route -> Html Msg
matchedRoute model path =
    case path of
        Home ->
            Home.content model

        -- TODO: maybeLevel should trigger different `update` api GET call,
        -- TODO: so this has to be refactored with routing as per:
        -- TODO: https://github.com/rtfeldman/elm-spa-example/blob/ad14ff6f8e50789ba59d8d2b17929f0737fc8373/src/Main.elm#L71
        Srs maybeLevel ->
            Srs.content maybeLevel
