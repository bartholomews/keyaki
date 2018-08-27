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

        --
        'K' :: 'A' :: ls ->
            addKana ls with 'カ'

        'k' :: 'a' :: ls ->
            addKana ls with 'か'

        'K' :: 'I' :: ls ->
            addKana ls with 'キ'

        'k' :: 'i' :: ls ->
            addKana ls with 'き'

        'K' :: 'U' :: ls ->
            addKana ls with 'ク'

        'k' :: 'u' :: ls ->
            addKana ls with 'く'

        'K' :: 'E' :: ls ->
            addKana ls with 'ケ'

        'k' :: 'e' :: ls ->
            addKana ls with 'け'

        'K' :: 'O' :: ls ->
            addKana ls with 'コ'

        'k' :: 'o' :: ls ->
            addKana ls with 'こ'

        'K' :: 'Y' :: 'A' :: ls ->
            addKanas ls with [ 'キ', 'ャ' ]

        'k' :: 'y' :: 'a' :: ls ->
            addKanas ls with [ 'き', 'ゃ' ]

        'K' :: 'Y' :: 'U' :: ls ->
            addKanas ls with [ 'キ', 'ュ' ]

        'k' :: 'y' :: 'u' :: ls ->
            addKanas ls with [ 'き', 'ゅ' ]

        'K' :: 'Y' :: 'O' :: ls ->
            addKanas ls with [ 'キ', 'ョ' ]

        'k' :: 'y' :: 'o' :: ls ->
            addKanas ls with [ 'き', 'ょ' ]

        --
        'N' :: ls ->
            addKana ls with 'ン'

        'n' :: 'i' :: ls ->
            addKana ls with 'に'

        'n' :: '\'' :: ls ->
            addKana ls with 'ん'

        'n' :: ls ->
            addKana ls with 'ん'

        --
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
