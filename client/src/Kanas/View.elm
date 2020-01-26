module Kanas.View exposing (itemView, listView)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Kanas.Types exposing (..)


listView : Kanas -> Visibility -> Html Msg
listView kanas visibility =
    let
        isVisible kanaItem =
            case visibility of
                Done ->
                    kanaItem.kana.completed

                Active ->
                    not kanaItem.kana.completed

                _ ->
                    True

        hasKanas =
            List.length kanas > 0
    in
    if hasKanas then
        ul [ class "list-reset m0" ]
            (List.map itemView <| List.filter isVisible kanas)

    else
        p [ class "center h1 gray regular px2 pt2 " ]
            [ text "¯\\_(ツ)_/¯" ]


itemView : KanaItem -> Html Msg
itemView item =
    let
        kana =
            item.kana

        editable =
            item.editable
    in
    li [ class "flex flex-center p2 border-bottom silver" ]
        [ div [ class "pr2" ]
            [ button
                [ class <|
                    "h6 regular italic btn m0 p0 pl1 pr1 white rounded"
                        ++ (if kana.completed then
                                " bg-green "

                            else
                                " bg-gray"
                           )
                , onClick <| ToggleKanaDone item
                ]
                [ text <|
                    if kana.completed then
                        "Done "

                    else
                        "Kana"
                ]
            ]
        , div
            [ class "flex-auto"
            ]
            [ input
                [ class <|
                    "block h1 col-12 navy "
                        ++ (if editable then
                                "border border-navy"

                            else
                                "muted border-none"
                           )
                , type_ "text"
                , disabled <| not editable
                , value kana.description
                , onInput (UpdateDescription item)
                ]
                []
            ]
        , if editable then
            div []
                [ button
                    [ class <|
                        "h4 regular btn btn-outline ml2 gray"
                    , onClick <| CancelEditKana item
                    ]
                    [ text "Cancel"
                    ]
                , button
                    [ class
                        "ml2 h4 regular btn btn-outline green"
                    , onClick <| UpdateKana item
                    ]
                    [ text "Update"
                    ]
                ]

          else
            div []
                [ button
                    [ class "ml2 h4 regular btn btn-outline fuchsia"
                    , onClick <| DeleteKana item
                    ]
                    [ text "Delete"
                    ]
                , button
                    [ class <|
                        "h4 regular btn btn-outline ml2 navy"
                    , onClick <| EditKana item
                    ]
                    [ text "Edit"
                    ]
                ]
        ]
