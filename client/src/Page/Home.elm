module Page.Home exposing (..)

import App.Types exposing (Model, Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Kana.View as KanaView
import Kanas.Types as KanasTypes
import Kanas.View as KanasView


content : Model -> Html Msg
content model =
    div []
        [ kanaForm model
        , visibilityMenu model.kanas model.kanasVisibility
        , Html.map KanasMsg <| KanasView.listView model.kanas model.kanasVisibility
        ]


kanaForm : Model -> Html Msg
kanaForm model =
    div [ class "center bg-black bg-cover bg-center header" ]
        [ div [ class "bg-darken-4 py4" ]
            [ Html.map KanaMsg <| KanaView.newKana <| .newKana model
            , resultView <| .kanas model
            ]
        ]


resultView : KanasTypes.Kanas -> Html Msg
resultView kanas =
    let
        kanasLength =
            List.length kanas

        kanasCompleted =
            List.length <|
                List.filter (\item -> .completed <| .kana <| item) kanas
    in
    p
        [ class <|
            "h6 p1 green muted inline-block "
                ++ (if kanasLength == 0 then
                        "hide"

                    else
                        ""
                   )
        ]
        [ text <| String.fromInt kanasCompleted ++ " / " ++ String.fromInt kanasLength ++ " done"
        ]


visibilityMenu : KanasTypes.Kanas -> KanasTypes.Visibility -> Html Msg
visibilityMenu kanas visibility =
    let
        isDisabled =
            List.length kanas == 0

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
            [ class <| menuBtnClass ++ activeClass visibility KanasTypes.All
            , onClick <| KanasMsg <| KanasTypes.SetVisibility KanasTypes.All
            , disabled isDisabled
            ]
            [ text "All"
            ]
        , button
            [ class <| menuBtnClass ++ activeClass visibility KanasTypes.Done
            , onClick <| KanasMsg <| KanasTypes.SetVisibility KanasTypes.Done
            , disabled isDisabled
            ]
            [ text "Done"
            ]
        , button
            [ class <| menuBtnClass ++ activeClass visibility KanasTypes.Active
            , onClick <| KanasMsg <| KanasTypes.SetVisibility KanasTypes.Active
            , disabled isDisabled
            ]
            [ text "Active"
            ]
        ]
