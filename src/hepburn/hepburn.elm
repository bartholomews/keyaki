module Hepburn exposing (kana)

import Debug exposing (log)
import List exposing (concat)
import String exposing (cons, uncons)


kana : String -> String
kana str =
    translate (String.toList str) []


addKana : List Char -> List Char -> Char -> String
addKana ls with symbol =
    translate
        ls
        (concat [ with, symbol :: [] ])


addKanas : List Char -> List Char -> List Char -> String
addKanas ls with symbols =
    translate
        ls
        (concat [ with, symbols ])


translate : List Char -> List Char -> String
translate current with =
    case current of
        --
        'A' :: ls ->
            addKana ls with 'ア'

        'a' :: ls ->
            addKana ls with 'あ'

        'I' :: ls ->
            addKana ls with 'イ'

        'i' :: ls ->
            addKana ls with 'い'

        'U' :: ls ->
            addKana ls with 'ウ'

        'u' :: ls ->
            addKana ls with 'う'

        'E' :: ls ->
            addKana ls with 'エ'

        'e' :: ls ->
            addKana ls with 'え'

        'O' :: ls ->
            addKana ls with 'オ'

        'o' :: ls ->
            addKana ls with 'お'

        _ ->
            String.fromList with



-- c :: xs ->
--     if addUnresolvedRomanji then
--         translate []
--             addUnresolvedRomanji
--             (concat
--                 [ acc, c :: xs ]
--             )
--
--     else
--         String.fromList acc
--
-- [] ->
--     String.fromList acc
