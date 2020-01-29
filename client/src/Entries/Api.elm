module Entries.Api exposing (getEntries)

import App.Types exposing (Config)
import Common.Url exposing (appendPath)
import Entries.Types exposing (..)
import Entry.Api exposing (entryDecoder)
import Entry.Types as Entry
import Http
import Json.Decode as Decode exposing (Decoder)
import Url exposing (Url)


getEntries : Config -> Cmd Msg
getEntries config =
    Http.get
        { url = Url.toString (appendPath config.serverApi "entries")
        , expect = Http.expectJson EntriesFetched entriesDecoder
        }



-- https://github.com/NoRedInk/elm-json-decode-pipeline


entriesDecoder : Decoder (List Entry.Entry)
entriesDecoder =
    Decode.list entryDecoder
