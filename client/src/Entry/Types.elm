module Entry.Types exposing (Entry, EntryRequest, Msg(..))

import Http


type alias Entry =
    { id : Int
    , active : Bool
    , kana : String
    , meaning : String
    }


type alias EntryRequest =
    { romaji : String, kana : Maybe String, meaning : String }


type Msg
    = UpdateKana String (Maybe String)
    | UpdateMeaning String
    | Save
    | Saved (Result Http.Error Entry)
    | Deleted (Result Http.Error String)
    | Updated (Result Http.Error String)
    | Cancel
    | NoOp
