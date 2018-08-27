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
        'S' :: 'A' :: ls ->
            addKana ls with 'サ'

        's' :: 'a' :: ls ->
            addKana ls with 'さ'

        'S' :: 'H' :: 'I' :: ls ->
            addKana ls with 'シ'

        's' :: 'h' :: 'i' :: ls ->
            addKana ls with 'し'

        'S' :: 'U' :: ls ->
            addKana ls with 'ス'

        's' :: 'u' :: ls ->
            addKana ls with 'す'

        'S' :: 'E' :: ls ->
            addKana ls with 'セ'

        's' :: 'e' :: ls ->
            addKana ls with 'せ'

        'S' :: 'O' :: ls ->
            addKana ls with 'ソ'

        's' :: 'o' :: ls ->
            addKana ls with 'そ'

        'S' :: 'H' :: 'A' :: ls ->
            addKanas ls with [ 'シ', 'ャ' ]

        's' :: 'h' :: 'a' :: ls ->
            addKanas ls with [ 'し', 'ゃ' ]

        'S' :: 'H' :: 'U' :: ls ->
            addKanas ls with [ 'シ', 'ュ' ]

        's' :: 'h' :: 'u' :: ls ->
            addKanas ls with [ 'し', 'ゅ' ]

        'S' :: 'H' :: 'O' :: ls ->
            addKanas ls with [ 'シ', 'ョ' ]

        's' :: 'h' :: 'o' :: ls ->
            addKanas ls with [ 'し', 'ょ' ]

        --
        'T' :: 'A' :: ls ->
            addKana ls with 'タ'

        't' :: 'a' :: ls ->
            addKana ls with 'た'

        'C' :: 'H' :: 'I' :: ls ->
            addKana ls with 'チ'

        'c' :: 'h' :: 'i' :: ls ->
            addKana ls with 'ち'

        'T' :: 'S' :: 'U' :: ls ->
            addKana ls with 'ツ'

        't' :: 's' :: 'u' :: ls ->
            addKana ls with 'つ'

        'T' :: 'E' :: ls ->
            addKana ls with 'テ'

        't' :: 'e' :: ls ->
            addKana ls with 'て'

        'T' :: 'O' :: ls ->
            addKana ls with 'ト'

        't' :: 'o' :: ls ->
            addKana ls with 'と'

        'C' :: 'H' :: 'A' :: ls ->
            addKanas ls with [ 'チ', 'ャ' ]

        'c' :: 'h' :: 'a' :: ls ->
            addKanas ls with [ 'ち', 'ゃ' ]

        'C' :: 'H' :: 'U' :: ls ->
            addKanas ls with [ 'チ', 'ュ' ]

        'c' :: 'h' :: 'u' :: ls ->
            addKanas ls with [ 'ち', 'ゅ' ]

        'C' :: 'H' :: 'O' :: ls ->
            addKanas ls with [ 'チ', 'ョ' ]

        'c' :: 'h' :: 'o' :: ls ->
            addKanas ls with [ 'ち', 'ょ' ]

        --
        'N' :: 'A' :: ls ->
            addKana ls with 'ナ'

        'n' :: 'a' :: ls ->
            addKana ls with 'な'

        'N' :: 'I' :: ls ->
            addKana ls with 'ニ'

        'n' :: 'i' :: ls ->
            addKana ls with 'に'

        'N' :: 'U' :: ls ->
            addKana ls with 'ヌ'

        'n' :: 'u' :: ls ->
            addKana ls with 'ぬ'

        'N' :: 'E' :: ls ->
            addKana ls with 'ネ'

        'n' :: 'e' :: ls ->
            addKana ls with 'ね'

        'N' :: 'O' :: ls ->
            addKana ls with 'ノ'

        'n' :: 'o' :: ls ->
            addKana ls with 'の'

        'N' :: 'Y' :: 'A' :: ls ->
            addKanas ls with [ 'ニ', 'ャ' ]

        'n' :: 'y' :: 'a' :: ls ->
            addKanas ls with [ 'に', 'ゃ' ]

        'N' :: 'Y' :: 'U' :: ls ->
            addKanas ls with [ 'ニ', 'ュ' ]

        'n' :: 'y' :: 'u' :: ls ->
            addKanas ls with [ 'に', 'ゅ' ]

        'N' :: 'Y' :: 'O' :: ls ->
            addKanas ls with [ 'ニ', 'ョ' ]

        'n' :: 'y' :: 'o' :: ls ->
            addKanas ls with [ 'に', 'ょ' ]

        --
        'H' :: 'A' :: ls ->
            addKana ls with 'ハ'

        'h' :: 'a' :: ls ->
            addKana ls with 'は'

        'H' :: 'I' :: ls ->
            addKana ls with 'ヒ'

        'h' :: 'i' :: ls ->
            addKana ls with 'ひ'

        'F' :: 'U' :: ls ->
            addKana ls with 'フ'

        'f' :: 'u' :: ls ->
            addKana ls with 'ふ'

        'H' :: 'E' :: ls ->
            addKana ls with 'ヘ'

        'h' :: 'e' :: ls ->
            addKana ls with 'へ'

        'H' :: 'O' :: ls ->
            addKana ls with 'ホ'

        'h' :: 'o' :: ls ->
            addKana ls with 'ほ'

        'H' :: 'Y' :: 'A' :: ls ->
            addKanas ls with [ 'ヒ', 'ャ' ]

        'h' :: 'y' :: 'a' :: ls ->
            addKanas ls with [ 'ひ', 'ゃ' ]

        'H' :: 'Y' :: 'U' :: ls ->
            addKanas ls with [ 'ヒ', 'ュ' ]

        'h' :: 'y' :: 'u' :: ls ->
            addKanas ls with [ 'ひ', 'ゅ' ]

        'H' :: 'Y' :: 'O' :: ls ->
            addKanas ls with [ 'ヒ', 'ョ' ]

        'h' :: 'y' :: 'o' :: ls ->
            addKanas ls with [ 'ひ', 'ょ' ]

        --
        'N' :: '\'' :: ls ->
            addKana ls with 'ン'

        'N' :: ls ->
            addKana ls with 'ン'

        'n' :: '\'' :: ls ->
            addKana ls with 'ん'

        'n' :: ls ->
            addKana ls with 'ん'

        --
        'M' :: 'A' :: ls ->
            addKana ls with 'マ'

        'm' :: 'a' :: ls ->
            addKana ls with 'ま'

        'M' :: 'I' :: ls ->
            addKana ls with 'ミ'

        'm' :: 'i' :: ls ->
            addKana ls with 'み'

        'M' :: 'U' :: ls ->
            addKana ls with 'ム'

        'm' :: 'u' :: ls ->
            addKana ls with 'む'

        'M' :: 'E' :: ls ->
            addKana ls with 'メ'

        'm' :: 'e' :: ls ->
            addKana ls with 'め'

        'M' :: 'O' :: ls ->
            addKana ls with 'モ'

        'm' :: 'o' :: ls ->
            addKana ls with 'も'

        'M' :: 'Y' :: 'A' :: ls ->
            addKanas ls with [ 'ミ', 'ャ' ]

        'm' :: 'y' :: 'a' :: ls ->
            addKanas ls with [ 'み', 'ゃ' ]

        'M' :: 'Y' :: 'U' :: ls ->
            addKanas ls with [ 'ミ', 'ュ' ]

        'm' :: 'y' :: 'u' :: ls ->
            addKanas ls with [ 'み', 'ゅ' ]

        'M' :: 'Y' :: 'O' :: ls ->
            addKanas ls with [ 'ミ', 'ョ' ]

        'm' :: 'y' :: 'o' :: ls ->
            addKanas ls with [ 'み', 'ょ' ]

        --
        'Y' :: 'A' :: ls ->
            addKana ls with 'ヤ'

        'y' :: 'a' :: ls ->
            addKana ls with 'や'

        'Y' :: 'U' :: ls ->
            addKana ls with 'ユ'

        'y' :: 'u' :: ls ->
            addKana ls with 'ゆ'

        'Y' :: 'O' :: ls ->
            addKana ls with 'ヨ'

        'y' :: 'o' :: ls ->
            addKana ls with 'よ'

        --
        'R' :: 'A' :: ls ->
            addKana ls with 'ラ'

        'r' :: 'a' :: ls ->
            addKana ls with 'ら'

        'R' :: 'I' :: ls ->
            addKana ls with 'リ'

        'r' :: 'i' :: ls ->
            addKana ls with 'り'

        'R' :: 'U' :: ls ->
            addKana ls with 'ル'

        'r' :: 'u' :: ls ->
            addKana ls with 'る'

        'R' :: 'E' :: ls ->
            addKana ls with 'レ'

        'r' :: 'e' :: ls ->
            addKana ls with 'れ'

        'R' :: 'O' :: ls ->
            addKana ls with 'ロ'

        'r' :: 'o' :: ls ->
            addKana ls with 'ろ'

        'R' :: 'Y' :: 'A' :: ls ->
            addKanas ls with [ 'リ', 'ャ' ]

        'r' :: 'y' :: 'a' :: ls ->
            addKanas ls with [ 'り', 'ゃ' ]

        'R' :: 'Y' :: 'U' :: ls ->
            addKanas ls with [ 'リ', 'ュ' ]

        'r' :: 'y' :: 'u' :: ls ->
            addKanas ls with [ 'り', 'ゅ' ]

        'R' :: 'Y' :: 'O' :: ls ->
            addKanas ls with [ 'リ', 'ョ' ]

        'r' :: 'y' :: 'o' :: ls ->
            addKanas ls with [ 'り', 'ょ' ]

        --
        'W' :: 'A' :: ls ->
            addKana ls with 'ワ'

        'w' :: 'a' :: ls ->
            addKana ls with 'わ'

        'W' :: 'I' :: ls ->
            addKana ls with 'ヰ'

        'w' :: 'i' :: ls ->
            addKana ls with 'ゐ'

        'W' :: 'E' :: ls ->
            addKana ls with 'ヱ'

        'w' :: 'e' :: ls ->
            addKana ls with 'ゑ'

        'W' :: 'O' :: ls ->
            addKana ls with 'ヲ'

        'w' :: 'o' :: ls ->
            addKana ls with 'を'

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
