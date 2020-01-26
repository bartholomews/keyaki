module App.Route exposing (..)

import App.Types exposing (HeaderLink, HeaderLinkName(..))
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s)
import Url.Parser.Query as Query



-- https://guide.elm-lang.org/webapps/url_parsing.html
-- /home
-- /srs?jltp=5
-- TODO: Add default redirect either to custom 404 page or `/home`


type Route
    = Home
    | Srs (Maybe Int)


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home (s "home")
        , map Srs (s "srs" <?> Query.int "jlpt") -- see Query #custom for a custom 1-5
        ]


matches : Maybe Route -> HeaderLink -> Bool
matches maybeRoute routeLink =
    case maybeRoute of
        Just Home ->
            routeLink.name == HOME

        Just (Srs _) ->
            routeLink.name == SRS

        Nothing ->
            False


stringifyLinkName : HeaderLinkName -> String
stringifyLinkName linkName =
    case linkName of
        HOME ->
            "HOME"

        SRS ->
            "SRS"



--
--toRoute : Url -> Route
--toRoute url =
--    case url of
--        Nothing ->
--            NotFound
--
--        Just url ->
--            Maybe.withDefault NotFound (parse route url)
