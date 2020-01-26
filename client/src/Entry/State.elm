module Entry.State exposing (emptyEntryRequest, initialNewEntry, update)

import Entry.Types exposing (..)
import Return exposing (Return)



-- Model


emptyEntryRequest : EntryRequest
emptyEntryRequest =
    EntryRequest "" Nothing


initialNewEntry : EntryRequest
initialNewEntry =
    emptyEntryRequest



-- update


update : Msg -> EntryRequest -> Return Msg EntryRequest
update msg entryRequest =
    Return.singleton entryRequest
        |> (case msg of
                -- FIXME just return req
                Update romaji maybeKana ->
                    Return.map (\entry_ -> { entry_ | romaji = romaji, kana = maybeKana })

                Cancel ->
                    Return.map <| always emptyEntryRequest

                _ ->
                    Return.zero
           )
