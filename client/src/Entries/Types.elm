module Entries.Types exposing (Entries, EntryItem, EntryItems, Msg(..), Visibility(..))

import Entry.Types as Entry
import Http


type alias EntryItem =
    { entry : Entry.Entry
    , romaji : String
    , editable : Bool
    }


type alias EntryItems =
    List EntryItem


type alias Entries =
    List EntryItem


type Visibility
    = All
    | Done
    | Active


type Msg
    = ToggleEntryDone EntryItem
    | EditEntry EntryItem
    | CancelEditEntry EntryItem
    | UpdateRomaji EntryItem String
    | UpdateEntry EntryItem
    | DeleteEntry EntryItem
    | EntriesFetched (Result Http.Error (List Entry.Entry))
    | SetVisibility Visibility
