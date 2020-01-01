module Page.NotFound exposing (..)

import App.Types exposing (Msg)
import Html exposing (Html, h1, text)



--type NotFoundMsg
--    = NF


content : Html Msg
content =
    h1 [] [ text "Sorry cant do anything" ]
