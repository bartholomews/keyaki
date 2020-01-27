module Entry.Types exposing (Entry, EntryRequest, Msg(..))

import Http


type alias Entry =
    { id : Int
    , active : Bool
    , kana : String
    }


type alias EntryRequest =
    { romaji : String, kana : Maybe String }


type Msg
    = Update String (Maybe String)
    | Save
    | Saved (Result Http.Error Entry)
    | Deleted (Result Http.Error String)
    | Updated (Result Http.Error String)
    | Cancel
    | NoOp
