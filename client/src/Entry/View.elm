module Entry.View exposing (newEntry, onKeyDown)

import Entry.Types exposing (Entry, EntryRequest, Msg(..))
import Hepburn.Translate exposing (romajiToKana)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Decode
import String


render : Maybe String -> String
render maybeKana =
    case maybeKana of
        Just kana ->
            kana

        Nothing ->
            ""


newEntry : EntryRequest -> Html Msg
newEntry nEntry =
    let
        maybeKana =
            romajiToKana nEntry.romaji

        isInvalid : EntryRequest -> Bool
        isInvalid entry =
            (String.isEmpty <| .romaji entry)
                || (String.isEmpty <| .meaning entry)
                || Maybe.withDefault True (Maybe.map (\_ -> False) maybeKana)
    in
    div [ class "clearfix mt3 mb3" ]
        [ h1 [ class "2 regular caps silver" ]
            [ text "ケヤキ" ]
        , input
            [ class "col-10 field h2 p2 mt2 mb2 border-none navy"
            , type_ "text"
            , value nEntry.romaji
            , placeholder "Rōmaji to Kana"
            , onInput (\romaji -> UpdateKana romaji (romajiToKana romaji))
            , autofocus True
            , onKeyDown
            ]
            []
        , h1 [ class "2 regular caps silver" ]
            [ text (render maybeKana) ]
        , div []
            [ input
                [ class "col-10 field h2 p2 mt2 mb2 border-none navy"
                , type_ "text"
                , value nEntry.meaning
                , placeholder "Meaning"
                , onInput UpdateMeaning
                , autofocus True
                , onKeyDown
                ]
                []
            ]
        , div [ class "" ]
            [ button
                [ class "h3 px4 py2 btn btn-outline lime"
                , onClick Save
                , disabled <| isInvalid nEntry
                ]
                [ text "Add Kana" ]
            ]
        , button
            [ class <|
                "btn  h5 regular silver underline"
                    ++ (if isInvalid nEntry then
                            " muted"

                        else
                            ""
                       )
            , onClick Cancel
            , disabled <| (String.isEmpty <| .romaji nEntry) && (String.isEmpty <| .meaning nEntry)
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
