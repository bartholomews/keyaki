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
                                , romaji = entry.kana
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
                                , romaji = ""
                                , entry = { entry_ | kana = .romaji entryItem }
                            }
                    in
                    Return.map <| updateEntryItem entryItem_

                UpdateEntry entryItem ->
                    let
                        entryItem_ =
                            { entryItem
                                | editable = False
                                , romaji = ""
                            }
                    in
                    Return.map <| updateEntryItem entryItem_

                UpdateRomaji entryItem romaji ->
                    let
                        entry_ =
                            .entry entryItem

                        entryItem_ =
                            { entryItem
                                | entry = { entry_ | kana = romaji }
                            }
                    in
                    Return.map <| updateEntryItem entryItem_

                _ ->
                    Return.zero
           )



-- state helper functions


createEntryItem : Entry.Entry -> EntryItem
createEntryItem entry =
    EntryItem entry "FIXME THIS SHOULD NOT BE NEEDED!" False


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
