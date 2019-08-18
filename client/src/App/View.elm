module App.View exposing (kanaForm, resultView, root, visibilityMenu)

import App.Types exposing (Model, Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Todo.View as TodoView
import Todos.Types as TodosTypes
import Todos.View as TodosView


root : Model -> Html Msg
root model =
    div []
        [ kanaForm model
        , visibilityMenu model.todos model.todosVisibility
        , Html.map TodosMsg <| TodosView.listView model.todos model.todosVisibility
        ]


kanaForm : Model -> Html Msg
kanaForm model =
    div [ class "center bg-black bg-cover bg-center header" ]
        [ div [ class "bg-darken-4 py4" ]
            [ Html.map TodoMsg <| TodoView.newTodo <| .newTodo model
            , resultView <| .todos model
            ]
        ]


resultView : TodosTypes.Todos -> Html Msg
resultView todos =
    let
        todosLength =
            List.length todos

        todosCompleted =
            List.length <|
                List.filter (\item -> .completed <| .todo <| item) todos
    in
    p
        [ class <|
            "h6 p1 green muted inline-block "
                ++ (if todosLength == 0 then
                        "hide"

                    else
                        ""
                   )
        ]
        [ text <| String.fromInt todosCompleted ++ " / " ++ String.fromInt todosLength ++ " done"
        ]


visibilityMenu : TodosTypes.Todos -> TodosTypes.Visibility -> Html Msg
visibilityMenu todos visibility =
    let
        isDisabled =
            List.length todos == 0

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
            [ class <| menuBtnClass ++ activeClass visibility TodosTypes.All
            , onClick <| TodosMsg <| TodosTypes.SetVisibility TodosTypes.All
            , disabled isDisabled
            ]
            [ text "All"
            ]
        , button
            [ class <| menuBtnClass ++ activeClass visibility TodosTypes.Done
            , onClick <| TodosMsg <| TodosTypes.SetVisibility TodosTypes.Done
            , disabled isDisabled
            ]
            [ text "Done"
            ]
        , button
            [ class <| menuBtnClass ++ activeClass visibility TodosTypes.Active
            , onClick <| TodosMsg <| TodosTypes.SetVisibility TodosTypes.Active
            , disabled isDisabled
            ]
            [ text "Active"
            ]
        ]
