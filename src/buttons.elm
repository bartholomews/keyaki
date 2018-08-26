module Main exposing (Model, main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { message : String, count : Int, clicked : Bool }


init : Model
init =
    { message = "", count = 0, clicked = False }



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { message = "Increment", count = model.count + 1, clicked = True }

        Decrement ->
            { message = "Decrement", count = model.count - 1, clicked = True }

        Reset ->
            { message = "Reset", count = 0, clicked = True }


isIncrement : Msg -> Bool
isIncrement msg =
    case msg of
        Increment ->
            True

        _ ->
            False


isEmpty : String -> Bool
isEmpty str =
    if str == "" then
        True

    else
        False



-- VIEW


messageFrom : Model -> String
messageFrom model =
    if isEmpty model.message then
        ""

    else
        "Last move => " ++ model.message


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (messageFrom model) ]
        , if model.clicked then
            button [ onClick Reset ] [ text "Reset" ]

          else
            div [] []
        , div [] [ text ("Count => " ++ String.fromInt model.count) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
