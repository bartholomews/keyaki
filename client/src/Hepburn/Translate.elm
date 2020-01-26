module Hepburn.Translate exposing (romajiToKana)

import List exposing (concat)
import String



-- exposing (cons, uncons)


romajiToKana : String -> Maybe String
romajiToKana romaji =
    translate (String.toList romaji)
        []



-- https://en.wikipedia.org/wiki/Hepburn_romanization


addKana : List Char -> List Char -> Char -> Maybe String
addKana rest acc current =
    translate
        rest
        (concat [ acc, current :: [] ])


addKanas : List Char -> List Char -> List Char -> Maybe String
addKanas rest acc current =
    translate
        rest
        (concat [ acc, current ])



-- separator should probably be customizable or function arg
-- TODO: tsu / zu postfix and doubles
--separator =
--    '\t'


translate : List Char -> List Char -> Maybe String
translate current acc =
    case current of
        --[ A ]---------------------------------------------------------------------------------------------------------
        'A' :: rest ->
            addKana rest acc 'ア'

        'a' :: rest ->
            addKana rest acc 'あ'

        'I' :: rest ->
            addKana rest acc 'イ'

        'i' :: rest ->
            addKana rest acc 'い'

        'U' :: rest ->
            addKana rest acc 'ウ'

        'u' :: rest ->
            addKana rest acc 'う'

        'E' :: rest ->
            addKana rest acc 'エ'

        'e' :: rest ->
            addKana rest acc 'え'

        'O' :: rest ->
            addKana rest acc 'オ'

        'o' :: rest ->
            addKana rest acc 'お'

        --[ KA ]--------------------------------------------------------------------------------------------------------
        'K' :: 'A' :: rest ->
            addKana rest acc 'カ'

        'k' :: 'a' :: rest ->
            addKana rest acc 'か'

        'K' :: 'I' :: rest ->
            addKana rest acc 'キ'

        'k' :: 'i' :: rest ->
            addKana rest acc 'き'

        'K' :: 'U' :: rest ->
            addKana rest acc 'ク'

        'k' :: 'u' :: rest ->
            addKana rest acc 'く'

        'K' :: 'E' :: rest ->
            addKana rest acc 'ケ'

        'k' :: 'e' :: rest ->
            addKana rest acc 'け'

        'K' :: 'O' :: rest ->
            addKana rest acc 'コ'

        'k' :: 'o' :: rest ->
            addKana rest acc 'こ'

        'K' :: 'Y' :: 'A' :: rest ->
            addKanas rest acc [ 'キ', 'ャ' ]

        'k' :: 'y' :: 'a' :: rest ->
            addKanas rest acc [ 'き', 'ゃ' ]

        'K' :: 'Y' :: 'U' :: rest ->
            addKanas rest acc [ 'キ', 'ュ' ]

        'k' :: 'y' :: 'u' :: rest ->
            addKanas rest acc [ 'き', 'ゅ' ]

        'K' :: 'Y' :: 'O' :: rest ->
            addKanas rest acc [ 'キ', 'ョ' ]

        'k' :: 'y' :: 'o' :: rest ->
            addKanas rest acc [ 'き', 'ょ' ]

        --[ SA ]--------------------------------------------------------------------------------------------------------
        'S' :: 'A' :: rest ->
            addKana rest acc 'サ'

        's' :: 'a' :: rest ->
            addKana rest acc 'さ'

        'S' :: 'H' :: 'I' :: rest ->
            addKana rest acc 'シ'

        's' :: 'h' :: 'i' :: rest ->
            addKana rest acc 'し'

        'S' :: 'U' :: rest ->
            addKana rest acc 'ス'

        's' :: 'u' :: rest ->
            addKana rest acc 'す'

        'S' :: 'E' :: rest ->
            addKana rest acc 'セ'

        's' :: 'e' :: rest ->
            addKana rest acc 'せ'

        'S' :: 'O' :: rest ->
            addKana rest acc 'ソ'

        's' :: 'o' :: rest ->
            addKana rest acc 'そ'

        'S' :: 'H' :: 'A' :: rest ->
            addKanas rest acc [ 'シ', 'ャ' ]

        's' :: 'h' :: 'a' :: rest ->
            addKanas rest acc [ 'し', 'ゃ' ]

        'S' :: 'H' :: 'U' :: rest ->
            addKanas rest acc [ 'シ', 'ュ' ]

        's' :: 'h' :: 'u' :: rest ->
            addKanas rest acc [ 'し', 'ゅ' ]

        'S' :: 'H' :: 'O' :: rest ->
            addKanas rest acc [ 'シ', 'ョ' ]

        's' :: 'h' :: 'o' :: rest ->
            addKanas rest acc [ 'し', 'ょ' ]

        --[ TA ]--------------------------------------------------------------------------------------------------------
        'T' :: 'A' :: rest ->
            addKana rest acc 'タ'

        't' :: 'a' :: rest ->
            addKana rest acc 'た'

        'C' :: 'H' :: 'I' :: rest ->
            addKana rest acc 'チ'

        'c' :: 'h' :: 'i' :: rest ->
            addKana rest acc 'ち'

        'T' :: 'S' :: 'U' :: rest ->
            addKana rest acc 'ツ'

        't' :: 's' :: 'u' :: rest ->
            addKana rest acc 'つ'

        'T' :: 'E' :: rest ->
            addKana rest acc 'テ'

        't' :: 'e' :: rest ->
            addKana rest acc 'て'

        'T' :: 'O' :: rest ->
            addKana rest acc 'ト'

        't' :: 'o' :: rest ->
            addKana rest acc 'と'

        'C' :: 'H' :: 'A' :: rest ->
            addKanas rest acc [ 'チ', 'ャ' ]

        'c' :: 'h' :: 'a' :: rest ->
            addKanas rest acc [ 'ち', 'ゃ' ]

        'C' :: 'H' :: 'U' :: rest ->
            addKanas rest acc [ 'チ', 'ュ' ]

        'c' :: 'h' :: 'u' :: rest ->
            addKanas rest acc [ 'ち', 'ゅ' ]

        'C' :: 'H' :: 'O' :: rest ->
            addKanas rest acc [ 'チ', 'ョ' ]

        'c' :: 'h' :: 'o' :: rest ->
            addKanas rest acc [ 'ち', 'ょ' ]

        --[ NA ]--------------------------------------------------------------------------------------------------------
        'N' :: 'A' :: rest ->
            addKana rest acc 'ナ'

        'n' :: 'a' :: rest ->
            addKana rest acc 'な'

        'N' :: 'I' :: rest ->
            addKana rest acc 'ニ'

        'n' :: 'i' :: rest ->
            addKana rest acc 'に'

        'N' :: 'U' :: rest ->
            addKana rest acc 'ヌ'

        'n' :: 'u' :: rest ->
            addKana rest acc 'ぬ'

        'N' :: 'E' :: rest ->
            addKana rest acc 'ネ'

        'n' :: 'e' :: rest ->
            addKana rest acc 'ね'

        'N' :: 'O' :: rest ->
            addKana rest acc 'ノ'

        'n' :: 'o' :: rest ->
            addKana rest acc 'の'

        'N' :: 'Y' :: 'A' :: rest ->
            addKanas rest acc [ 'ニ', 'ャ' ]

        'n' :: 'y' :: 'a' :: rest ->
            addKanas rest acc [ 'に', 'ゃ' ]

        'N' :: 'Y' :: 'U' :: rest ->
            addKanas rest acc [ 'ニ', 'ュ' ]

        'n' :: 'y' :: 'u' :: rest ->
            addKanas rest acc [ 'に', 'ゅ' ]

        'N' :: 'Y' :: 'O' :: rest ->
            addKanas rest acc [ 'ニ', 'ョ' ]

        'n' :: 'y' :: 'o' :: rest ->
            addKanas rest acc [ 'に', 'ょ' ]

        --[ HA ]--------------------------------------------------------------------------------------------------------
        'H' :: 'A' :: rest ->
            addKana rest acc 'ハ'

        'h' :: 'a' :: rest ->
            addKana rest acc 'は'

        'H' :: 'I' :: rest ->
            addKana rest acc 'ヒ'

        'h' :: 'i' :: rest ->
            addKana rest acc 'ひ'

        'F' :: 'U' :: rest ->
            addKana rest acc 'フ'

        'f' :: 'u' :: rest ->
            addKana rest acc 'ふ'

        'H' :: 'E' :: rest ->
            addKana rest acc 'ヘ'

        'h' :: 'e' :: rest ->
            addKana rest acc 'へ'

        'H' :: 'O' :: rest ->
            addKana rest acc 'ホ'

        'h' :: 'o' :: rest ->
            addKana rest acc 'ほ'

        'H' :: 'Y' :: 'A' :: rest ->
            addKanas rest acc [ 'ヒ', 'ャ' ]

        'h' :: 'y' :: 'a' :: rest ->
            addKanas rest acc [ 'ひ', 'ゃ' ]

        'H' :: 'Y' :: 'U' :: rest ->
            addKanas rest acc [ 'ヒ', 'ュ' ]

        'h' :: 'y' :: 'u' :: rest ->
            addKanas rest acc [ 'ひ', 'ゅ' ]

        'H' :: 'Y' :: 'O' :: rest ->
            addKanas rest acc [ 'ヒ', 'ョ' ]

        'h' :: 'y' :: 'o' :: rest ->
            addKanas rest acc [ 'ひ', 'ょ' ]

        --[ MA ]--------------------------------------------------------------------------------------------------------
        'M' :: 'A' :: rest ->
            addKana rest acc 'マ'

        'm' :: 'a' :: rest ->
            addKana rest acc 'ま'

        'M' :: 'I' :: rest ->
            addKana rest acc 'ミ'

        'm' :: 'i' :: rest ->
            addKana rest acc 'み'

        'M' :: 'U' :: rest ->
            addKana rest acc 'ム'

        'm' :: 'u' :: rest ->
            addKana rest acc 'む'

        'M' :: 'E' :: rest ->
            addKana rest acc 'メ'

        'm' :: 'e' :: rest ->
            addKana rest acc 'め'

        'M' :: 'O' :: rest ->
            addKana rest acc 'モ'

        'm' :: 'o' :: rest ->
            addKana rest acc 'も'

        'M' :: 'Y' :: 'A' :: rest ->
            addKanas rest acc [ 'ミ', 'ャ' ]

        'm' :: 'y' :: 'a' :: rest ->
            addKanas rest acc [ 'み', 'ゃ' ]

        'M' :: 'Y' :: 'U' :: rest ->
            addKanas rest acc [ 'ミ', 'ュ' ]

        'm' :: 'y' :: 'u' :: rest ->
            addKanas rest acc [ 'み', 'ゅ' ]

        'M' :: 'Y' :: 'O' :: rest ->
            addKanas rest acc [ 'ミ', 'ョ' ]

        'm' :: 'y' :: 'o' :: rest ->
            addKanas rest acc [ 'み', 'ょ' ]

        --[ YA ]--------------------------------------------------------------------------------------------------------
        'Y' :: 'A' :: rest ->
            addKana rest acc 'ヤ'

        'y' :: 'a' :: rest ->
            addKana rest acc 'や'

        'Y' :: 'U' :: rest ->
            addKana rest acc 'ユ'

        'y' :: 'u' :: rest ->
            addKana rest acc 'ゆ'

        'Y' :: 'O' :: rest ->
            addKana rest acc 'ヨ'

        'y' :: 'o' :: rest ->
            addKana rest acc 'よ'

        --[ RA ]--------------------------------------------------------------------------------------------------------
        'R' :: 'A' :: rest ->
            addKana rest acc 'ラ'

        'r' :: 'a' :: rest ->
            addKana rest acc 'ら'

        'R' :: 'I' :: rest ->
            addKana rest acc 'リ'

        'r' :: 'i' :: rest ->
            addKana rest acc 'り'

        'R' :: 'U' :: rest ->
            addKana rest acc 'ル'

        'r' :: 'u' :: rest ->
            addKana rest acc 'る'

        'R' :: 'E' :: rest ->
            addKana rest acc 'レ'

        'r' :: 'e' :: rest ->
            addKana rest acc 'れ'

        'R' :: 'O' :: rest ->
            addKana rest acc 'ロ'

        'r' :: 'o' :: rest ->
            addKana rest acc 'ろ'

        'R' :: 'Y' :: 'A' :: rest ->
            addKanas rest acc [ 'リ', 'ャ' ]

        'r' :: 'y' :: 'a' :: rest ->
            addKanas rest acc [ 'り', 'ゃ' ]

        'R' :: 'Y' :: 'U' :: rest ->
            addKanas rest acc [ 'リ', 'ュ' ]

        'r' :: 'y' :: 'u' :: rest ->
            addKanas rest acc [ 'り', 'ゅ' ]

        'R' :: 'Y' :: 'O' :: rest ->
            addKanas rest acc [ 'リ', 'ョ' ]

        'r' :: 'y' :: 'o' :: rest ->
            addKanas rest acc [ 'り', 'ょ' ]

        --[ N ]---------------------------------------------------------------------------------------------------------
        'N' :: '\'' :: rest ->
            addKana rest acc 'ン'

        'N' :: rest ->
            addKana rest acc 'ン'

        'n' :: '\'' :: rest ->
            addKana rest acc 'ん'

        'n' :: rest ->
            addKana rest acc 'ん'

        --[ GA ]--------------------------------------------------------------------------------------------------------
        'G' :: 'A' :: rest ->
            addKana rest acc 'ガ'

        'g' :: 'a' :: rest ->
            addKana rest acc 'が'

        'G' :: 'I' :: rest ->
            addKana rest acc 'ギ'

        'g' :: 'i' :: rest ->
            addKana rest acc 'ぎ'

        'G' :: 'U' :: rest ->
            addKana rest acc 'グ'

        'g' :: 'u' :: rest ->
            addKana rest acc 'ぐ'

        'G' :: 'E' :: rest ->
            addKana rest acc 'ゲ'

        'g' :: 'e' :: rest ->
            addKana rest acc 'げ'

        'G' :: 'O' :: rest ->
            addKana rest acc 'ゴ'

        'g' :: 'o' :: rest ->
            addKana rest acc 'ご'

        'G' :: 'Y' :: 'A' :: rest ->
            addKanas rest acc [ 'ギ', 'ャ' ]

        'g' :: 'y' :: 'a' :: rest ->
            addKanas rest acc [ 'ぎ', 'ゃ' ]

        'G' :: 'Y' :: 'U' :: rest ->
            addKanas rest acc [ 'ギ', 'ュ' ]

        'g' :: 'y' :: 'u' :: rest ->
            addKanas rest acc [ 'ぎ', 'ゅ' ]

        'G' :: 'Y' :: 'O' :: rest ->
            addKanas rest acc [ 'ギ', 'ョ' ]

        'g' :: 'y' :: 'o' :: rest ->
            addKanas rest acc [ 'ぎ', 'ょ' ]

        --[ DA ]--------------------------------------------------------------------------------------------------------
        'D' :: 'A' :: rest ->
            addKana rest acc 'ダ'

        'd' :: 'a' :: rest ->
            addKana rest acc 'だ'

        'J' :: 'I' :: '\'' :: rest ->
            addKana rest acc 'ヂ'

        'j' :: 'i' :: '\'' :: rest ->
            addKana rest acc 'ぢ'

        'Z' :: 'U' :: '\'' :: rest ->
            addKana rest acc 'ヅ'

        'z' :: 'u' :: '\'' :: rest ->
            addKana rest acc 'づ'

        'D' :: 'E' :: rest ->
            addKana rest acc 'デ'

        'd' :: 'e' :: rest ->
            addKana rest acc 'で'

        'D' :: 'O' :: rest ->
            addKana rest acc 'ド'

        'd' :: 'o' :: rest ->
            addKana rest acc 'ど'

        'J' :: 'A' :: '\'' :: rest ->
            addKanas rest acc [ 'ヂ', 'ャ' ]

        'j' :: 'a' :: '\'' :: rest ->
            addKanas rest acc [ 'ぢ', 'ゃ' ]

        'J' :: 'U' :: '\'' :: rest ->
            addKanas rest acc [ 'ヂ', 'ュ' ]

        'j' :: 'u' :: '\'' :: rest ->
            addKanas rest acc [ 'ぢ', 'ゅ' ]

        'J' :: 'O' :: '\'' :: rest ->
            addKanas rest acc [ 'ヂ', 'ョ' ]

        'j' :: 'o' :: '\'' :: rest ->
            addKanas rest acc [ 'ぢ', 'ょ' ]

        --[ ZA ]--------------------------------------------------------------------------------------------------------
        'Z' :: 'A' :: rest ->
            addKana rest acc 'ザ'

        'z' :: 'a' :: rest ->
            addKana rest acc 'ざ'

        'J' :: 'I' :: rest ->
            addKana rest acc 'ジ'

        'j' :: 'i' :: rest ->
            addKana rest acc 'じ'

        'Z' :: 'U' :: rest ->
            addKana rest acc 'ズ'

        'z' :: 'u' :: rest ->
            addKana rest acc 'ず'

        'Z' :: 'E' :: rest ->
            addKana rest acc 'ゼ'

        'z' :: 'e' :: rest ->
            addKana rest acc 'ぜ'

        'Z' :: 'O' :: rest ->
            addKana rest acc 'ゾ'

        'z' :: 'o' :: rest ->
            addKana rest acc 'ぞ'

        'J' :: 'A' :: rest ->
            addKanas rest acc [ 'ジ', 'ャ' ]

        'j' :: 'a' :: rest ->
            addKanas rest acc [ 'じ', 'ゃ' ]

        'J' :: 'U' :: rest ->
            addKanas rest acc [ 'ジ', 'ュ' ]

        'j' :: 'u' :: rest ->
            addKanas rest acc [ 'じ', 'ゅ' ]

        'J' :: 'O' :: rest ->
            addKanas rest acc [ 'ジ', 'ョ' ]

        'j' :: 'o' :: rest ->
            addKanas rest acc [ 'じ', 'ょ' ]

        --[ BA ]--------------------------------------------------------------------------------------------------------
        'B' :: 'A' :: rest ->
            addKana rest acc 'バ'

        'b' :: 'a' :: rest ->
            addKana rest acc 'ば'

        'B' :: 'I' :: rest ->
            addKana rest acc 'ビ'

        'b' :: 'i' :: rest ->
            addKana rest acc 'び'

        'B' :: 'U' :: rest ->
            addKana rest acc 'ブ'

        'b' :: 'u' :: rest ->
            addKana rest acc 'ぶ'

        'B' :: 'E' :: rest ->
            addKana rest acc 'ベ'

        'b' :: 'e' :: rest ->
            addKana rest acc 'べ'

        'B' :: 'O' :: rest ->
            addKana rest acc 'ボ'

        'b' :: 'o' :: rest ->
            addKana rest acc 'ぼ'

        'B' :: 'Y' :: 'A' :: rest ->
            addKanas rest acc [ 'ビ', 'ャ' ]

        'b' :: 'y' :: 'a' :: rest ->
            addKanas rest acc [ 'び', 'ゃ' ]

        'B' :: 'Y' :: 'U' :: rest ->
            addKanas rest acc [ 'ビ', 'ュ' ]

        'b' :: 'y' :: 'u' :: rest ->
            addKanas rest acc [ 'び', 'ゅ' ]

        'B' :: 'Y' :: 'O' :: rest ->
            addKanas rest acc [ 'ビ', 'ョ' ]

        'b' :: 'y' :: 'o' :: rest ->
            addKanas rest acc [ 'び', 'ょ' ]

        --[ PA ]--------------------------------------------------------------------------------------------------------
        'P' :: 'A' :: rest ->
            addKana rest acc 'パ'

        'p' :: 'a' :: rest ->
            addKana rest acc 'ぱ'

        'P' :: 'I' :: rest ->
            addKana rest acc 'ピ'

        'p' :: 'i' :: rest ->
            addKana rest acc 'ぴ'

        'P' :: 'U' :: rest ->
            addKana rest acc 'プ'

        'p' :: 'u' :: rest ->
            addKana rest acc 'ぷ'

        'P' :: 'E' :: rest ->
            addKana rest acc 'ペ'

        'p' :: 'e' :: rest ->
            addKana rest acc 'ぺ'

        'P' :: 'O' :: rest ->
            addKana rest acc 'ポ'

        'p' :: 'o' :: rest ->
            addKana rest acc 'ぽ'

        'P' :: 'Y' :: 'A' :: rest ->
            addKanas rest acc [ 'ピ', 'ャ' ]

        'p' :: 'y' :: 'a' :: rest ->
            addKanas rest acc [ 'ぴ', 'ゃ' ]

        'P' :: 'Y' :: 'U' :: rest ->
            addKanas rest acc [ 'ピ', 'ュ' ]

        'p' :: 'y' :: 'u' :: rest ->
            addKanas rest acc [ 'ぴ', 'ゅ' ]

        'P' :: 'Y' :: 'O' :: rest ->
            addKanas rest acc [ 'ピ', 'ョ' ]

        'p' :: 'y' :: 'o' :: rest ->
            addKanas rest acc [ 'ぴ', 'ょ' ]

        --[ YI ]--------------------------------------------------------------------------------------------------------
        'Y' :: 'I' :: rest ->
            addKanas rest acc [ 'イ', 'ィ' ]

        'Y' :: 'E' :: rest ->
            addKanas rest acc [ 'イ', 'ェ' ]

        'W' :: 'I' :: '\'' :: rest ->
            addKanas rest acc [ 'ウ', 'ィ' ]

        'W' :: 'U' :: rest ->
            addKanas rest acc [ 'ウ', 'ゥ' ]

        'W' :: 'E' :: '\'' :: rest ->
            addKanas rest acc [ 'ウ', 'ェ' ]

        'W' :: 'O' :: '\'' :: rest ->
            addKanas rest acc [ 'ウ', 'ォ' ]

        'V' :: 'A' :: rest ->
            addKanas rest acc [ 'ヴ', 'ァ' ]

        'V' :: 'I' :: rest ->
            addKanas rest acc [ 'ヴ', 'ィ' ]

        'V' :: 'U' :: rest ->
            addKana rest acc 'ヴ'

        'V' :: 'E' :: rest ->
            addKanas rest acc [ 'ヴ', 'ェ' ]

        'V' :: 'O' :: rest ->
            addKanas rest acc [ 'ヴ', 'ォ' ]

        'V' :: 'Y' :: 'U' :: rest ->
            addKanas rest acc [ 'ヴ', 'ュ' ]

        'K' :: 'W' :: 'A' :: rest ->
            addKanas rest acc [ 'ク', 'ァ' ]

        'K' :: 'W' :: 'I' :: '\'' :: rest ->
            addKanas rest acc [ 'ク', 'ィ' ]

        'K' :: 'W' :: 'E' :: rest ->
            addKanas rest acc [ 'ク', 'ェ' ]

        'K' :: 'W' :: 'O' :: rest ->
            addKanas rest acc [ 'ク', 'ォ' ]

        'G' :: 'W' :: 'A' :: rest ->
            addKanas rest acc [ 'グ', 'ァ' ]

        'S' :: 'H' :: 'E' :: rest ->
            addKanas rest acc [ 'シ', 'ェ' ]

        'J' :: 'E' :: rest ->
            addKanas rest acc [ 'ジ', 'ェ' ]

        'S' :: 'I' :: rest ->
            addKanas rest acc [ 'ス', 'ィ' ]

        'Z' :: 'I' :: rest ->
            addKanas rest acc [ 'ズ', 'ィ' ]

        'C' :: 'H' :: 'E' :: rest ->
            addKanas rest acc [ 'チ', 'ェ' ]

        'T' :: 'S' :: 'A' :: rest ->
            addKanas rest acc [ 'ツ', 'ァ' ]

        'T' :: 'S' :: 'I' :: rest ->
            addKanas rest acc [ 'ツ', 'ィ' ]

        'T' :: 'S' :: 'E' :: rest ->
            addKanas rest acc [ 'ツ', 'ェ' ]

        'T' :: 'S' :: 'O' :: rest ->
            addKanas rest acc [ 'ツ', 'ォ' ]

        'T' :: 'I' :: rest ->
            addKanas rest acc [ 'テ', 'ィ' ]

        'T' :: 'U' :: rest ->
            addKanas rest acc [ 'テ', 'ゥ' ]

        'T' :: 'Y' :: 'U' :: rest ->
            addKanas rest acc [ 'テ', 'ュ' ]

        'D' :: 'I' :: rest ->
            addKanas rest acc [ 'デ', 'ィ' ]

        'D' :: 'U' :: rest ->
            addKanas rest acc [ 'デ', 'ゥ' ]

        'D' :: 'Y' :: 'U' :: rest ->
            addKanas rest acc [ 'デ', 'ュ' ]

        'F' :: 'A' :: rest ->
            addKanas rest acc [ 'フ', 'ァ' ]

        'F' :: 'I' :: rest ->
            addKanas rest acc [ 'フ', 'ィ' ]

        'F' :: 'E' :: rest ->
            addKanas rest acc [ 'フ', 'ェ' ]

        'F' :: 'O' :: rest ->
            addKanas rest acc [ 'フ', 'ォ' ]

        'F' :: 'Y' :: 'U' :: rest ->
            addKanas rest acc [ 'フ', 'ュ' ]

        'H' :: 'U' :: rest ->
            addKanas rest acc [ 'ホ', 'ゥ' ]

        'L' :: 'A' :: rest ->
            addKanas rest acc [ 'ラ', '゜' ]

        'L' :: 'I' :: rest ->
            addKanas rest acc [ 'リ', '゜' ]

        'L' :: 'U' :: rest ->
            addKanas rest acc [ 'ル', '゜' ]

        'L' :: 'E' :: rest ->
            addKanas rest acc [ 'レ', '゜' ]

        'L' :: 'O' :: rest ->
            addKanas rest acc [ 'ロ', '゜' ]

        --[ WA ]--------------------------------------------------------------------------------------------------------
        'W' :: 'A' :: rest ->
            addKana rest acc 'ワ'

        'w' :: 'a' :: rest ->
            addKana rest acc 'わ'

        'W' :: 'I' :: rest ->
            addKana rest acc 'ヰ'

        'w' :: 'i' :: rest ->
            addKana rest acc 'ゐ'

        'W' :: 'E' :: rest ->
            addKana rest acc 'ヱ'

        'w' :: 'e' :: rest ->
            addKana rest acc 'ゑ'

        'W' :: 'O' :: rest ->
            addKana rest acc 'ヲ'

        'w' :: 'o' :: rest ->
            addKana rest acc 'を'

        [] ->
            Just (String.fromList acc)

        _ ->
            Nothing
