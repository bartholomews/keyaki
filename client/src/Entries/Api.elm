module Entries.Api exposing (getEntries)

import Entries.Types exposing (..)
import Entry.Api exposing (entryDecoder)
import Entry.Types as Entry
import Http
import Json.Decode as Decode exposing (Decoder)


getEntries : Cmd Msg
getEntries =
    Http.get
        { url = "http://localhost:8081/api/entries"
        , expect = Http.expectJson EntriesFetched entriesDecoder
        }



-- https://github.com/NoRedInk/elm-json-decode-pipeline


entriesDecoder : Decoder (List Entry.Entry)
entriesDecoder =
    Decode.list entryDecoder
