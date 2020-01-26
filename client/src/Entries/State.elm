module Entries.State exposing (createEntryItem, deleteEntryItem, initialEntries, initialVisibility, update, updateEntry, updateEntryItem)

import Entries.Types exposing (..)
import Entry.Types as Entry
import Return exposing (Return)



-- Model


initialVisibility : Visibility
initialVisibility =
    All


initialEntries : Entries
initialEntries =
    []



-- update


update : Msg -> Entries -> Return Msg Entries
update msg entries =
    Return.singleton entries
        |> (case msg of
                EditEntry entryItem ->
                    let
                        entry =
                            entryItem.entry

                        entryItem_ =
                            { entryItem
                                | editable = True
                                , romanji = entry.romanji
                            }
                    in
                    Return.map <| updateEntryItem entryItem_

                CancelEditEntry entryItem ->
                    let
                        entry_ =
                            .entry entryItem

                        entryItem_ =
                            { entryItem
                                | editable = False
                                , romanji = ""
                                , entry = { entry_ | romanji = .romanji entryItem }
                            }
                    in
                    Return.map <| updateEntryItem entryItem_

                UpdateEntry entryItem ->
                    let
                        entryItem_ =
                            { entryItem
                                | editable = False
                                , romanji = ""
                            }
                    in
                    Return.map <| updateEntryItem entryItem_

                UpdateRomanji entryItem romanji ->
                    let
                        entry_ =
                            .entry entryItem

                        entryItem_ =
                            { entryItem
                                | entry = { entry_ | romanji = romanji }
                            }
                    in
                    Return.map <| updateEntryItem entryItem_

                _ ->
                    Return.zero
           )



-- state helper functions


createEntryItem : Entry.Entry -> EntryItem
createEntryItem entry =
    EntryItem entry "" False


deleteEntryItem : EntryItem -> Entries -> Entries
deleteEntryItem entryItem =
    List.filter <| (/=) entryItem


updateEntry : Entry.Entry -> Entries -> Entries
updateEntry entry =
    List.map
        (\entryItem ->
            if entryItem.entry.id == entry.id then
                { entryItem | entry = entry }

            else
                entryItem
        )


updateEntryItem : EntryItem -> Entries -> Entries
updateEntryItem entryItem =
    List.map
        (\entryItem_ ->
            if entryItem_.entry.id == entryItem.entry.id then
                entryItem

            else
                entryItem_
        )



--
--getEntryItem : EntryItem -> Entries -> Maybe EntryItem
--getEntryItem entryItem entries =
--    List.head <|
--        List.filter
--            (\entryItem_ ->
--                entryItem_.entry.id == entryItem.entry.id
--            )
--            entries
