module Kana.Types exposing (Kana, Msg(..))

import Http


type alias Kana =
    { id : Int
    , completed : Bool
    , description : String
    }


type Msg
    = Update String
    | Save
    | Saved (Result Http.Error Int)
    | Deleted (Result Http.Error String)
    | Updated (Result Http.Error String)
    | Cancel
    | NoOp
