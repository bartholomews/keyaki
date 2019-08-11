module Todo.Types exposing (Msg(..), Todo)

import Http


type alias Todo =
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
