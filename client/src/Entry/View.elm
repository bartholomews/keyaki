module Entry.View exposing (newEntry, onKeyDown)

import Entry.Types exposing (Entry, Msg(..))
import Hepburn.Translate exposing (romanjiToKana)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Decode
import String


newEntry : Entry -> Html Msg
newEntry nEntry =
    let
        hasEmptyRomanji : Entry -> Bool
        hasEmptyRomanji entry =
            String.isEmpty <| .romanji entry
    in
    div [ class "clearfix mt3 mb3" ]
        [ h1 [ class "2 regular caps silver" ]
            [ text "ケヤキ" ]
        , input
            [ class "col-10 field h2 p2 mt2 mb2 border-none navy"
            , type_ "text"
            , value nEntry.romanji
            , placeholder "Rōmaji to Kana"
            , onInput Update
            , autofocus True
            , onKeyDown
            ]
            []
        , h1 [ class "2 regular caps silver" ]
            [ text (romanjiToKana nEntry.romanji) ]
        , div [ class "" ]
            [ button
                [ class "h3 px4 py2 btn btn-outline lime"
                , onClick Save
                , disabled <| hasEmptyRomanji nEntry
                ]
                [ text "Add Kana" ]
            ]
        , button
            [ class <|
                "btn  h5 regular silver underline"
                    ++ (if hasEmptyRomanji nEntry then
                            " muted"

                        else
                            ""
                       )
            , onClick Cancel
            , disabled <| hasEmptyRomanji nEntry
            ]
            [ text "Clear" ]
        ]


onKeyDown : Attribute Msg
onKeyDown =
    let
        keyDecoder keyCode_ =
            case keyCode_ of
                13 ->
                    Save

                27 ->
                    Cancel

                _ ->
                    NoOp
    in
    on "keydown" <| Decode.map keyDecoder keyCode
