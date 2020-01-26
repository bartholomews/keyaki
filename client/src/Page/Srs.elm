module Page.Srs exposing (..)

import App.Types exposing (Msg)
import Html exposing (..)
import Maybe as M exposing (withDefault)
import String exposing (concat)


content : Maybe Int -> Html Msg
content maybeLevel =
    let
        jlptLevel =
            maybeLevel
                |> M.map String.fromInt
                |> M.map (\level -> concat [ "\nJLPT LEVEL: ", level ])
                |> withDefault ""
    in
    div []
        [ h1 []
            [ text (concat [ "SRS", " [TODO]" ]) ]
        , h3 []
            [ text jlptLevel ]
        ]
