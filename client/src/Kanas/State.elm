module Kanas.State exposing (createKanaItem, deleteKanaItem, initialKanas, initialVisibility, update, updateKana, updateKanaItem)

import Kana.Types as Kana
import Kanas.Types exposing (..)
import Return exposing (Return)



-- Model


initialVisibility : Visibility
initialVisibility =
    All


initialKanas : Kanas
initialKanas =
    []



-- update


update : Msg -> Kanas -> Return Msg Kanas
update msg kanas =
    Return.singleton kanas
        |> (case msg of
                EditKana kanaItem ->
                    let
                        kana =
                            kanaItem.kana

                        kanaItem_ =
                            { kanaItem
                                | editable = True
                                , description = kana.description
                            }
                    in
                    Return.map <| updateKanaItem kanaItem_

                CancelEditKana kanaItem ->
                    let
                        kana_ =
                            .kana kanaItem

                        kanaItem_ =
                            { kanaItem
                                | editable = False
                                , description = ""
                                , kana = { kana_ | description = .description kanaItem }
                            }
                    in
                    Return.map <| updateKanaItem kanaItem_

                UpdateKana kanaItem ->
                    let
                        kanaItem_ =
                            { kanaItem
                                | editable = False
                                , description = ""
                            }
                    in
                    Return.map <| updateKanaItem kanaItem_

                UpdateDescription kanaItem description ->
                    let
                        kana_ =
                            .kana kanaItem

                        kanaItem_ =
                            { kanaItem
                                | kana = { kana_ | description = description }
                            }
                    in
                    Return.map <| updateKanaItem kanaItem_

                _ ->
                    Return.zero
           )



-- state helper functions


createKanaItem : Kana.Kana -> KanaItem
createKanaItem kana =
    KanaItem kana "" False


deleteKanaItem : KanaItem -> Kanas -> Kanas
deleteKanaItem kanaItem =
    List.filter <| (/=) kanaItem


updateKana : Kana.Kana -> Kanas -> Kanas
updateKana kana =
    List.map
        (\kanaItem ->
            if kanaItem.kana.id == kana.id then
                { kanaItem | kana = kana }

            else
                kanaItem
        )


updateKanaItem : KanaItem -> Kanas -> Kanas
updateKanaItem kanaItem =
    List.map
        (\kanaItem_ ->
            if kanaItem_.kana.id == kanaItem.kana.id then
                kanaItem

            else
                kanaItem_
        )



--
--getKanaItem : KanaItem -> Kanas -> Maybe KanaItem
--getKanaItem kanaItem kanas =
--    List.head <|
--        List.filter
--            (\kanaItem_ ->
--                kanaItem_.kana.id == kanaItem.kana.id
--            )
--            kanas
