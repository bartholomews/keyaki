module Page.Home exposing (..)

import App.Types exposing (Model, Msg(..))
import Entries.Types as EntriesTypes
import Entries.View as EntriesView
import Entry.View as EntryView
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


content : Model -> Html Msg
content model =
    div []
        [ entryForm model
        , visibilityMenu model.entries model.entriesVisibility
        , Html.map EntriesMsg <| EntriesView.listView model.entries model.entriesVisibility
        ]


entryForm : Model -> Html Msg
entryForm model =
    div [ class "center bg-black bg-cover bg-center header" ]
        [ div [ class "bg-darken-4 py4" ]
            [ Html.map EntryMsg <| EntryView.newEntry <| .newEntry model
            , resultView <| .entries model
            ]
        ]


resultView : EntriesTypes.Entries -> Html Msg
resultView entries =
    let
        entriesLength =
            List.length entries

        entriesActive =
            List.length <|
                List.filter (\item -> .active <| .entry <| item) entries
    in
    p
        [ class <|
            "h6 p1 green muted inline-block "
                ++ (if entriesLength == 0 then
                        "hide"

                    else
                        ""
                   )
        ]
        [ text <| String.fromInt entriesActive ++ " / " ++ String.fromInt entriesLength ++ " active"
        ]


visibilityMenu : EntriesTypes.Entries -> EntriesTypes.Visibility -> Html Msg
visibilityMenu entries visibility =
    let
        isDisabled =
            List.length entries == 0

        activeClass visib value =
            if visib == value then
                " bg-silver gray "

            else
                ""

        menuBtnClass =
            "flex-auto h4 py2 regular btn btn-primary bg-gray white not-rounded "
    in
    div
        [ class "flex center "
        ]
        [ button
            [ class <| menuBtnClass ++ activeClass visibility EntriesTypes.All
            , onClick <| EntriesMsg <| EntriesTypes.SetVisibility EntriesTypes.All
            , disabled isDisabled
            ]
            [ text "All"
            ]
        , button
            [ class <| menuBtnClass ++ activeClass visibility EntriesTypes.Done
            , onClick <| EntriesMsg <| EntriesTypes.SetVisibility EntriesTypes.Done
            , disabled isDisabled
            ]
            [ text "Done"
            ]
        , button
            [ class <| menuBtnClass ++ activeClass visibility EntriesTypes.Active
            , onClick <| EntriesMsg <| EntriesTypes.SetVisibility EntriesTypes.Active
            , disabled isDisabled
            ]
            [ text "Active"
            ]
        ]
