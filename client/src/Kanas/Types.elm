module Kanas.Types exposing (KanaItem, KanaItems, Kanas, Msg(..), Visibility(..))

import Http
import Kana.Types as Kana


type alias KanaItem =
    { kana : Kana.Kana
    , description : String
    , editable : Bool
    }


type alias KanaItems =
    List KanaItem


type alias Kanas =
    List KanaItem


type Visibility
    = All
    | Done
    | Active


type Msg
    = ToggleKanaDone KanaItem
    | EditKana KanaItem
    | CancelEditKana KanaItem
    | UpdateDescription KanaItem String
    | UpdateKana KanaItem
    | DeleteKana KanaItem
    | KanasFetched (Result Http.Error (List Kana.Kana))
    | SetVisibility Visibility
