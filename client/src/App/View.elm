module App.View exposing (kanaForm, resultView, root, rootOld, visibilityMenu)

import App.Route exposing (Route(..), route)
import App.Types exposing (Model, Msg(..))
import Common.Html exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.NotFound as NotFound
import Page.Srs as Srs
import Todo.View as TodoView
import Todos.Types as TodosTypes
import Todos.View as TodosView
import Url.Parser exposing (parse)


navigation : Html Msg
navigation =
    nav []
        [ ul []
            [ li [] [ anchorLink "home" Nothing ]
            , li [] [ anchorLink "srs?jlpt=1" (Just "srs") ]
            ]
        ]


matchedRoute : Model -> Route -> Html Msg
matchedRoute model path =
    case path of
        Home ->
            rootOld model

        -- TODO: maybeLevel should trigger different `update` api GET call,
        -- TODO: so this has to be refactored with routing as per:
        -- TODO: https://github.com/rtfeldman/elm-spa-example/blob/ad14ff6f8e50789ba59d8d2b17929f0737fc8373/src/Main.elm#L71
        SRS maybeLevel ->
            Srs.content maybeLevel

        NotFound ->
            NotFound.content

        Todo _ ->
            NotFound.content


content : Model -> Html Msg
content model =
    main_ [ class "main-content" ]
        [ case parse route model.url of
            Just path ->
                matchedRoute model path

            Nothing ->
                NotFound.content
        ]



-- https://stackoverflow.com/questions/52544763/how-to-create-spa-with-elm-0-19


root : Model -> Html Msg
root model =
    div []
        [ div [ class "header-content" ]
            [ header [] [ text "Header" ]
            , navigation
            ]
        , content model
        ]


rootOld : Model -> Html Msg
rootOld model =
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
