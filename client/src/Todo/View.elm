module Todo.View exposing (newTodo, onKeyDown)

import App.Hepburn exposing (kana)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Decode
import String
import Todo.Types exposing (Msg(..), Todo)


newTodo : Todo -> Html Msg
newTodo nTodo =
    let
        hasEmptyDescription : Todo -> Bool
        hasEmptyDescription todo =
            String.isEmpty <| .description todo
    in
    div [ class "clearfix mt3 mb3" ]
        [ h1 [ class "2 regular caps silver" ]
            [ text "ケヤキ" ]
        , input
            [ class "col-10 field h2 p2 mt2 mb2 border-none navy"
            , type_ "text"
            , value nTodo.description
            , placeholder "Rōmaji to Kana"
            , onInput Update
            , autofocus True
            , onKeyDown
            ]
            []
        , h1 [ class "2 regular caps silver" ]
            [ text (kana nTodo.description) ]
        , div [ class "" ]
            [ button
                [ class "h3 px4 py2 btn btn-outline lime"
                , onClick Save
                , disabled <| hasEmptyDescription nTodo
                ]
                [ text "Add Kana" ]
            ]
        , button
            [ class <|
                "btn  h5 regular silver underline"
                    ++ (if hasEmptyDescription nTodo then
                            " muted"

                        else
                            ""
                       )
            , onClick Cancel
            , disabled <| hasEmptyDescription nTodo
            ]
            [ text "Or skip" ]
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