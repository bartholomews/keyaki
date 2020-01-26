module App.Route exposing (..)

import App.Types exposing (LinkName(..), RouteLink)
import Url.Parser exposing ((</>), (<?>), Parser, int, map, oneOf, s)
import Url.Parser.Query as Query



-- https://guide.elm-lang.org/webapps/url_parsing.html
-- /home
-- /srs?jltp=5
-- /test/
-- /todo/1
-- TODO: Add default redirect either to custom 404 page or `/home`


type Route
    = Home
    | Srs (Maybe Int)
    | Todo Int


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home (s "home")
        , map Srs (s "srs" <?> Query.int "jlpt") -- see Query #custom for a custom 1-5
        , map Todo (s "todo" </> int)
        ]


matches : Maybe Route -> RouteLink -> Bool
matches maybeRoute routeLink =
    case maybeRoute of
        Just Home ->
            routeLink.linkName == HOME

        Just (Srs _) ->
            routeLink.linkName == SRS

        Just (Todo _) ->
            routeLink.linkName == TODO

        Nothing ->
            False


stringifyLinkName : LinkName -> String
stringifyLinkName linkName =
    case linkName of
        HOME ->
            "HOME"

        SRS ->
            "SRS"

        TODO ->
            "TODO"



--
--toRoute : Url -> Route
--toRoute url =
--    case url of
--        Nothing ->
--            NotFound
--
--        Just url ->
--            Maybe.withDefault NotFound (parse route url)
