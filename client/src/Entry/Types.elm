module Entry.Types exposing (Entry, Msg(..))

import Http


type alias Entry =
    { id : Int
    , active : Bool
    , romanji : String
    }


type Msg
    = Update String
    | Save
    | Saved (Result Http.Error Int)
    | Deleted (Result Http.Error String)
    | Updated (Result Http.Error String)
    | Cancel
    | NoOp
