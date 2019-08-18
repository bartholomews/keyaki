module Page.Srs exposing (..)

import App.Types exposing (Msg)
import Html exposing (..)
import Maybe exposing (map, withDefault)
import String exposing (concat)


content : Maybe Int -> Html Msg
content maybeLevel =
    let
        jlptLevel =
            maybeLevel
                |> map String.fromInt
                |> map (\level -> concat [ "\nJLPT LEVEL: ", level ])
                |> withDefault ""
    in
    div []
        [ h1 []
            [ text (concat [ "SRS", " [TODO]" ]) ]
        , h3 []
            [ text jlptLevel ]
        ]
