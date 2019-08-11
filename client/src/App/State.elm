module App.State exposing (initialCommand, initialModel, update, updateTodo, updateTodos)

import App.Types exposing (..)
import Return exposing (Return)
import Todo.Api as Todo
import Todo.State as TodoState
import Todo.Types as TodoTypes
import Todos.Api as TodosApi
import Todos.State as TodosState
import Todos.Types as TodosTypes



-- Model


initialModel : Model
initialModel =
    { todos = TodosState.initialTodos
    , todosVisibility = TodosState.initialVisibility
    , newTodo = TodoState.initialNewTodo
    }



-- Cmd


initialCommand : Cmd Msg
initialCommand =
    Cmd.map TodosMsg TodosApi.getTodos



-- update


update : Msg -> Model -> Return Msg Model
update msg model =
    Return.singleton model
        |> (case msg of
                TodosMsg msg_ ->
                    updateTodos msg_

                TodoMsg msg_ ->
                    updateTodo msg_
           )


updateTodo : TodoTypes.Msg -> Return Msg Model -> Return Msg Model
updateTodo msg writer =
    let
        ( appModel, _ ) =
            writer

        ( todoModel, todoCmd ) =
            TodoState.update msg appModel.newTodo
    in
    writer
        --|> (case Debug.log "t " msg of
        |> (case msg of
                TodoTypes.Save ->
                    Return.command (Cmd.map TodoMsg <| Todo.saveTodo todoModel)

                TodoTypes.Saved (Ok todoId) ->
                    let
                        todo =
                            { todoModel | id = todoId }

                        todos =
                            List.append appModel.todos [ TodosState.createTodoItem todo ]
                    in
                    Return.map
                        (\m -> { m | todos = todos, newTodo = TodoState.emptyTodo })

                _ ->
                    Return.mapWith (\m -> { m | newTodo = todoModel }) <|
                        Cmd.map TodoMsg todoCmd
           )


updateTodos : TodosTypes.Msg -> Return Msg Model -> Return Msg Model
updateTodos msg writer =
    let
        ( appModel, _ ) =
            writer

        ( todosModel, todosCmd ) =
            TodosState.update msg appModel.todos
    in
    writer
        --|> (case Debug.log "ts " msg of
        |> (case msg of
                TodosTypes.TodosFetched (Ok todos) ->
                    Return.map <|
                        \m -> { m | todos = List.map TodosState.createTodoItem todos }

                TodosTypes.DeleteTodo todoItem ->
                    Return.mapWith
                        (\m -> { m | todos = TodosState.deleteTodoItem todoItem todosModel })
                        (Cmd.map TodoMsg <|
                            Todo.deleteTodo todoItem.todo
                        )

                TodosTypes.UpdateTodo todoItem ->
                    Return.mapWith
                        (\m -> { m | todos = todosModel })
                        (Cmd.map TodoMsg <|
                            Todo.updateTodo <|
                                .todo todoItem
                        )

                TodosTypes.ToggleTodoDone todoItem ->
                    let
                        todo =
                            todoItem.todo

                        todo_ =
                            { todo | completed = not todo.completed }
                    in
                    Return.mapWith
                        (\m -> { m | todos = TodosState.updateTodo todo_ todosModel })
                        (Cmd.map TodoMsg <|
                            Todo.updateTodo todo_
                        )

                TodosTypes.SetVisibility visibility ->
                    Return.map (\m -> { m | todosVisibility = visibility })

                _ ->
                    Return.mapWith
                        (\m -> { m | todos = todosModel })
                        (Cmd.map TodosMsg todosCmd)
           )
