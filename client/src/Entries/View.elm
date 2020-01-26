module Entries.View exposing (itemView, listView)

import Entries.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


listView : Entries -> Visibility -> Html Msg
listView entries visibility =
    let
        isVisible entryItem =
            case visibility of
                Done ->
                    not entryItem.entry.active

                Active ->
                    entryItem.entry.active

                _ ->
                    True

        hasEntries =
            List.length entries > 0
    in
    if hasEntries then
        ul [ class "list-reset m0" ]
            (List.map itemView <| List.filter isVisible entries)

    else
        p [ class "center h1 gray regular px2 pt2 " ]
            [ text "¯\\_(ツ)_/¯" ]


itemView : EntryItem -> Html Msg
itemView item =
    let
        entry =
            item.entry

        editable =
            item.editable
    in
    li [ class "flex flex-center p2 border-bottom silver" ]
        [ div [ class "pr2" ]
            [ button
                [ class <|
                    "h6 regular italic btn m0 p0 pl1 pr1 white rounded"
                        ++ (if entry.active then
                                " bg-green "

                            else
                                " bg-gray"
                           )
                , onClick <| ToggleEntryDone item
                ]
                [ text <|
                    if entry.active then
                        "Active"

                    else
                        "Done"
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
                , value entry.romanji
                , onInput (UpdateRomanji item)
                ]
                []
            ]
        , if editable then
            div []
                [ button
                    [ class <|
                        "h4 regular btn btn-outline ml2 gray"
                    , onClick <| CancelEditEntry item
                    ]
                    [ text "Cancel"
                    ]
                , button
                    [ class
                        "ml2 h4 regular btn btn-outline green"
                    , onClick <| UpdateEntry item
                    ]
                    [ text "Update"
                    ]
                ]

          else
            div []
                [ button
                    [ class "ml2 h4 regular btn btn-outline fuchsia"
                    , onClick <| DeleteEntry item
                    ]
                    [ text "Delete"
                    ]
                , button
                    [ class <|
                        "h4 regular btn btn-outline ml2 navy"
                    , onClick <| EditEntry item
                    ]
                    [ text "Edit"
                    ]
                ]
        ]
