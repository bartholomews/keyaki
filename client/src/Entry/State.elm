module Entry.State exposing (emptyEntry, initialNewEntry, update)

import Entry.Types exposing (..)
import Return exposing (Return)



-- Model


emptyEntry : Entry
emptyEntry =
    Entry -1 False ""


initialNewEntry : Entry
initialNewEntry =
    emptyEntry



-- update


update : Msg -> Entry -> Return Msg Entry
update msg entry =
    Return.singleton entry
        |> (case msg of
                Update value ->
                    Return.map (\entry_ -> { entry_ | romanji = value })

                Cancel ->
                    Return.map <| always emptyEntry

                _ ->
                    Return.zero
           )
