module Kanas.Api exposing (getKanas)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Kana.Types as Kana
import Kanas.Types exposing (..)


getKanas : Cmd Msg
getKanas =
    Http.get
        { url = "http://localhost:8081/api/kanas/"
        , expect = Http.expectJson KanasFetched kanasDecoder
        }



-- https://github.com/NoRedInk/elm-json-decode-pipeline


kanaDecoder : Decoder Kana.Kana
kanaDecoder =
    Decode.succeed Kana.Kana
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "completed" Decode.bool
        |> Pipeline.required "description" Decode.string


kanasDecoder : Decoder (List Kana.Kana)
kanasDecoder =
    Decode.list kanaDecoder
