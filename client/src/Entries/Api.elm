module Entries.Api exposing (getEntries)

import Entries.Types exposing (..)
import Entry.Types as Entry
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


getEntries : Cmd Msg
getEntries =
    Http.get
        { url = "http://localhost:8081/api/entries/"
        , expect = Http.expectJson EntriesFetched entriesDecoder
        }



-- https://github.com/NoRedInk/elm-json-decode-pipeline


entryDecoder : Decoder Entry.Entry
entryDecoder =
    Decode.succeed Entry.Entry
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "active" Decode.bool
        |> Pipeline.required "romanji" Decode.string


entriesDecoder : Decoder (List Entry.Entry)
entriesDecoder =
    Decode.list entryDecoder
