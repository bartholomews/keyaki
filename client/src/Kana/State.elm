module Kana.State exposing (emptyKana, initialNewKana, update)

import Kana.Types exposing (..)
import Return exposing (Return)



-- Model


emptyKana : Kana
emptyKana =
    Kana -1 False ""


initialNewKana : Kana
initialNewKana =
    emptyKana



-- update


update : Msg -> Kana -> Return Msg Kana
update msg kana =
    Return.singleton kana
        |> (case msg of
                Update value ->
                    Return.map (\kana_ -> { kana_ | description = value })

                Cancel ->
                    Return.map <| always emptyKana

                _ ->
                    Return.zero
           )
