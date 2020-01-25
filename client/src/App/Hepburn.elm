module App.Hepburn exposing (kana)

-- import Debug exposing (log)

import List exposing (concat)
import String



-- exposing (cons, uncons)


kana : String -> String
kana str =
    translate (String.toList str)
        []



-- https://en.wikipedia.org/wiki/Hepburn_romanization


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



-- separator should probably be customizable or function arg
-- TODO: tsu / zu postfix and doubles
--separator =
--    '\t'


translate : List Char -> List Char -> String
translate current with =
    case current of
        --[ A ]---------------------------------------------------------------------------------------------------------
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

        --[ KA ]--------------------------------------------------------------------------------------------------------
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

        --[ SA ]--------------------------------------------------------------------------------------------------------
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

        --[ TA ]--------------------------------------------------------------------------------------------------------
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

        --[ NA ]--------------------------------------------------------------------------------------------------------
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

        --[ HA ]--------------------------------------------------------------------------------------------------------
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

        --[ MA ]--------------------------------------------------------------------------------------------------------
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

        --[ YA ]--------------------------------------------------------------------------------------------------------
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

        --[ RA ]--------------------------------------------------------------------------------------------------------
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

        --[ N ]---------------------------------------------------------------------------------------------------------
        'N' :: '\'' :: ls ->
            addKana ls with 'ン'

        'N' :: ls ->
            addKana ls with 'ン'

        'n' :: '\'' :: ls ->
            addKana ls with 'ん'

        'n' :: ls ->
            addKana ls with 'ん'

        --[ GA ]--------------------------------------------------------------------------------------------------------
        'G' :: 'A' :: ls ->
            addKana ls with 'ガ'

        'g' :: 'a' :: ls ->
            addKana ls with 'が'

        'G' :: 'I' :: ls ->
            addKana ls with 'ギ'

        'g' :: 'i' :: ls ->
            addKana ls with 'ぎ'

        'G' :: 'U' :: ls ->
            addKana ls with 'グ'

        'g' :: 'u' :: ls ->
            addKana ls with 'ぐ'

        'G' :: 'E' :: ls ->
            addKana ls with 'ゲ'

        'g' :: 'e' :: ls ->
            addKana ls with 'げ'

        'G' :: 'O' :: ls ->
            addKana ls with 'ゴ'

        'g' :: 'o' :: ls ->
            addKana ls with 'ご'

        'G' :: 'Y' :: 'A' :: ls ->
            addKanas ls with [ 'ギ', 'ャ' ]

        'g' :: 'y' :: 'a' :: ls ->
            addKanas ls with [ 'ぎ', 'ゃ' ]

        'G' :: 'Y' :: 'U' :: ls ->
            addKanas ls with [ 'ギ', 'ュ' ]

        'g' :: 'y' :: 'u' :: ls ->
            addKanas ls with [ 'ぎ', 'ゅ' ]

        'G' :: 'Y' :: 'O' :: ls ->
            addKanas ls with [ 'ギ', 'ョ' ]

        'g' :: 'y' :: 'o' :: ls ->
            addKanas ls with [ 'ぎ', 'ょ' ]

        --[ DA ]--------------------------------------------------------------------------------------------------------
        'D' :: 'A' :: ls ->
            addKana ls with 'ダ'

        'd' :: 'a' :: ls ->
            addKana ls with 'だ'

        'J' :: 'I' :: '\'' :: ls ->
            addKana ls with 'ヂ'

        'j' :: 'i' :: '\'' :: ls ->
            addKana ls with 'ぢ'

        'Z' :: 'U' :: '\'' :: ls ->
            addKana ls with 'ヅ'

        'z' :: 'u' :: '\'' :: ls ->
            addKana ls with 'づ'

        'D' :: 'E' :: ls ->
            addKana ls with 'デ'

        'd' :: 'e' :: ls ->
            addKana ls with 'で'

        'D' :: 'O' :: ls ->
            addKana ls with 'ド'

        'd' :: 'o' :: ls ->
            addKana ls with 'ど'

        'J' :: 'A' :: '\'' :: ls ->
            addKanas ls with [ 'ヂ', 'ャ' ]

        'j' :: 'a' :: '\'' :: ls ->
            addKanas ls with [ 'ぢ', 'ゃ' ]

        'J' :: 'U' :: '\'' :: ls ->
            addKanas ls with [ 'ヂ', 'ュ' ]

        'j' :: 'u' :: '\'' :: ls ->
            addKanas ls with [ 'ぢ', 'ゅ' ]

        'J' :: 'O' :: '\'' :: ls ->
            addKanas ls with [ 'ヂ', 'ョ' ]

        'j' :: 'o' :: '\'' :: ls ->
            addKanas ls with [ 'ぢ', 'ょ' ]

        --[ ZA ]--------------------------------------------------------------------------------------------------------
        'Z' :: 'A' :: ls ->
            addKana ls with 'ザ'

        'z' :: 'a' :: ls ->
            addKana ls with 'ざ'

        'J' :: 'I' :: ls ->
            addKana ls with 'ジ'

        'j' :: 'i' :: ls ->
            addKana ls with 'じ'

        'Z' :: 'U' :: ls ->
            addKana ls with 'ズ'

        'z' :: 'u' :: ls ->
            addKana ls with 'ず'

        'Z' :: 'E' :: ls ->
            addKana ls with 'ゼ'

        'z' :: 'e' :: ls ->
            addKana ls with 'ぜ'

        'Z' :: 'O' :: ls ->
            addKana ls with 'ゾ'

        'z' :: 'o' :: ls ->
            addKana ls with 'ぞ'

        'J' :: 'A' :: ls ->
            addKanas ls with [ 'ジ', 'ャ' ]

        'j' :: 'a' :: ls ->
            addKanas ls with [ 'じ', 'ゃ' ]

        'J' :: 'U' :: ls ->
            addKanas ls with [ 'ジ', 'ュ' ]

        'j' :: 'u' :: ls ->
            addKanas ls with [ 'じ', 'ゅ' ]

        'J' :: 'O' :: ls ->
            addKanas ls with [ 'ジ', 'ョ' ]

        'j' :: 'o' :: ls ->
            addKanas ls with [ 'じ', 'ょ' ]

        --[ BA ]--------------------------------------------------------------------------------------------------------
        'B' :: 'A' :: ls ->
            addKana ls with 'バ'

        'b' :: 'a' :: ls ->
            addKana ls with 'ば'

        'B' :: 'I' :: ls ->
            addKana ls with 'ビ'

        'b' :: 'i' :: ls ->
            addKana ls with 'び'

        'B' :: 'U' :: ls ->
            addKana ls with 'ブ'

        'b' :: 'u' :: ls ->
            addKana ls with 'ぶ'

        'B' :: 'E' :: ls ->
            addKana ls with 'ベ'

        'b' :: 'e' :: ls ->
            addKana ls with 'べ'

        'B' :: 'O' :: ls ->
            addKana ls with 'ボ'

        'b' :: 'o' :: ls ->
            addKana ls with 'ぼ'

        'B' :: 'Y' :: 'A' :: ls ->
            addKanas ls with [ 'ビ', 'ャ' ]

        'b' :: 'y' :: 'a' :: ls ->
            addKanas ls with [ 'び', 'ゃ' ]

        'B' :: 'Y' :: 'U' :: ls ->
            addKanas ls with [ 'ビ', 'ュ' ]

        'b' :: 'y' :: 'u' :: ls ->
            addKanas ls with [ 'び', 'ゅ' ]

        'B' :: 'Y' :: 'O' :: ls ->
            addKanas ls with [ 'ビ', 'ョ' ]

        'b' :: 'y' :: 'o' :: ls ->
            addKanas ls with [ 'び', 'ょ' ]

        --[ PA ]--------------------------------------------------------------------------------------------------------
        'P' :: 'A' :: ls ->
            addKana ls with 'パ'

        'p' :: 'a' :: ls ->
            addKana ls with 'ぱ'

        'P' :: 'I' :: ls ->
            addKana ls with 'ピ'

        'p' :: 'i' :: ls ->
            addKana ls with 'ぴ'

        'P' :: 'U' :: ls ->
            addKana ls with 'プ'

        'p' :: 'u' :: ls ->
            addKana ls with 'ぷ'

        'P' :: 'E' :: ls ->
            addKana ls with 'ペ'

        'p' :: 'e' :: ls ->
            addKana ls with 'ぺ'

        'P' :: 'O' :: ls ->
            addKana ls with 'ポ'

        'p' :: 'o' :: ls ->
            addKana ls with 'ぽ'

        'P' :: 'Y' :: 'A' :: ls ->
            addKanas ls with [ 'ピ', 'ャ' ]

        'p' :: 'y' :: 'a' :: ls ->
            addKanas ls with [ 'ぴ', 'ゃ' ]

        'P' :: 'Y' :: 'U' :: ls ->
            addKanas ls with [ 'ピ', 'ュ' ]

        'p' :: 'y' :: 'u' :: ls ->
            addKanas ls with [ 'ぴ', 'ゅ' ]

        'P' :: 'Y' :: 'O' :: ls ->
            addKanas ls with [ 'ピ', 'ョ' ]

        'p' :: 'y' :: 'o' :: ls ->
            addKanas ls with [ 'ぴ', 'ょ' ]

        --[ YI ]--------------------------------------------------------------------------------------------------------
        'Y' :: 'I' :: ls ->
            addKanas ls with [ 'イ', 'ィ' ]

        'Y' :: 'E' :: ls ->
            addKanas ls with [ 'イ', 'ェ' ]

        'W' :: 'I' :: '\'' :: ls ->
            addKanas ls with [ 'ウ', 'ィ' ]

        'W' :: 'U' :: ls ->
            addKanas ls with [ 'ウ', 'ゥ' ]

        'W' :: 'E' :: '\'' :: ls ->
            addKanas ls with [ 'ウ', 'ェ' ]

        'W' :: 'O' :: '\'' :: ls ->
            addKanas ls with [ 'ウ', 'ォ' ]

        'V' :: 'A' :: ls ->
            addKanas ls with [ 'ヴ', 'ァ' ]

        'V' :: 'I' :: ls ->
            addKanas ls with [ 'ヴ', 'ィ' ]

        'V' :: 'U' :: ls ->
            addKana ls with 'ヴ'

        'V' :: 'E' :: ls ->
            addKanas ls with [ 'ヴ', 'ェ' ]

        'V' :: 'O' :: ls ->
            addKanas ls with [ 'ヴ', 'ォ' ]

        'V' :: 'Y' :: 'U' :: ls ->
            addKanas ls with [ 'ヴ', 'ュ' ]

        'K' :: 'W' :: 'A' :: ls ->
            addKanas ls with [ 'ク', 'ァ' ]

        'K' :: 'W' :: 'I' :: '\'' :: ls ->
            addKanas ls with [ 'ク', 'ィ' ]

        'K' :: 'W' :: 'E' :: ls ->
            addKanas ls with [ 'ク', 'ェ' ]

        'K' :: 'W' :: 'O' :: ls ->
            addKanas ls with [ 'ク', 'ォ' ]

        'G' :: 'W' :: 'A' :: ls ->
            addKanas ls with [ 'グ', 'ァ' ]

        'S' :: 'H' :: 'E' :: ls ->
            addKanas ls with [ 'シ', 'ェ' ]

        'J' :: 'E' :: ls ->
            addKanas ls with [ 'ジ', 'ェ' ]

        'S' :: 'I' :: ls ->
            addKanas ls with [ 'ス', 'ィ' ]

        'Z' :: 'I' :: ls ->
            addKanas ls with [ 'ズ', 'ィ' ]

        'C' :: 'H' :: 'E' :: ls ->
            addKanas ls with [ 'チ', 'ェ' ]

        'T' :: 'S' :: 'A' :: ls ->
            addKanas ls with [ 'ツ', 'ァ' ]

        'T' :: 'S' :: 'I' :: ls ->
            addKanas ls with [ 'ツ', 'ィ' ]

        'T' :: 'S' :: 'E' :: ls ->
            addKanas ls with [ 'ツ', 'ェ' ]

        'T' :: 'S' :: 'O' :: ls ->
            addKanas ls with [ 'ツ', 'ォ' ]

        'T' :: 'I' :: ls ->
            addKanas ls with [ 'テ', 'ィ' ]

        'T' :: 'U' :: ls ->
            addKanas ls with [ 'テ', 'ゥ' ]

        'T' :: 'Y' :: 'U' :: ls ->
            addKanas ls with [ 'テ', 'ュ' ]

        'D' :: 'I' :: ls ->
            addKanas ls with [ 'デ', 'ィ' ]

        'D' :: 'U' :: ls ->
            addKanas ls with [ 'デ', 'ゥ' ]

        'D' :: 'Y' :: 'U' :: ls ->
            addKanas ls with [ 'デ', 'ュ' ]

        'F' :: 'A' :: ls ->
            addKanas ls with [ 'フ', 'ァ' ]

        'F' :: 'I' :: ls ->
            addKanas ls with [ 'フ', 'ィ' ]

        'F' :: 'E' :: ls ->
            addKanas ls with [ 'フ', 'ェ' ]

        'F' :: 'O' :: ls ->
            addKanas ls with [ 'フ', 'ォ' ]

        'F' :: 'Y' :: 'U' :: ls ->
            addKanas ls with [ 'フ', 'ュ' ]

        'H' :: 'U' :: ls ->
            addKanas ls with [ 'ホ', 'ゥ' ]

        'L' :: 'A' :: ls ->
            addKanas ls with [ 'ラ', '゜' ]

        'L' :: 'I' :: ls ->
            addKanas ls with [ 'リ', '゜' ]

        'L' :: 'U' :: ls ->
            addKanas ls with [ 'ル', '゜' ]

        'L' :: 'E' :: ls ->
            addKanas ls with [ 'レ', '゜' ]

        'L' :: 'O' :: ls ->
            addKanas ls with [ 'ロ', '゜' ]

        --[ WA ]--------------------------------------------------------------------------------------------------------
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

        _ ->
            String.fromList with



-- c :: xs ->
--     if addUnresolvedRomanji then -- TODO should disable `Add Kana` and show an error banner
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
