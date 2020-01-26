module App.State exposing (initialCommand, initialModel, update, updateKana, updateKanas)

import App.Types exposing (..)
import Browser
import Browser.Navigation as Nav
import Kana.Api as Kana
import Kana.State as KanaState
import Kana.Types as KanaTypes
import Kanas.Api as KanasApi
import Kanas.State as KanasState
import Kanas.Types as KanasTypes
import Return exposing (Return)
import Url exposing (Url)



-- Model


initialModel : Url -> Nav.Key -> Model
initialModel url key =
    { url = url
    , navKey = key
    , kanas = KanasState.initialKanas
    , kanasVisibility = KanasState.initialVisibility
    , newKana = KanaState.initialNewKana
    }



-- Cmd


initialCommand : Cmd Msg
initialCommand =
    Cmd.map KanasMsg KanasApi.getKanas



-- update


update : Msg -> Model -> Return Msg Model
update msg model =
    Return.singleton model
        |> (case msg of
                KanasMsg msg_ ->
                    updateKanas msg_

                KanaMsg msg_ ->
                    updateKana msg_

                UrlChanged newUrl ->
                    Return.map (\m -> { m | url = newUrl })

                LinkClicked urlRequest ->
                    case urlRequest of
                        Browser.Internal newUrl ->
                            Return.command (Nav.pushUrl model.navKey (Url.toString newUrl))

                        Browser.External href ->
                            Return.command (Nav.load href)

                DoNothing ->
                    Return.zero
           )


updateKana : KanaTypes.Msg -> Return Msg Model -> Return Msg Model
updateKana msg writer =
    let
        ( appModel, _ ) =
            writer

        ( kanaModel, kanaCmd ) =
            KanaState.update msg appModel.newKana
    in
    writer
        --|> (case Debug.log "t " msg of
        |> (case msg of
                KanaTypes.Save ->
                    Return.command (Cmd.map KanaMsg <| Kana.saveKana kanaModel)

                KanaTypes.Saved (Ok kanaId) ->
                    let
                        kana =
                            { kanaModel | id = kanaId }

                        kanas =
                            List.append appModel.kanas [ KanasState.createKanaItem kana ]
                    in
                    Return.map
                        (\m -> { m | kanas = kanas, newKana = KanaState.emptyKana })

                _ ->
                    Return.mapWith (\m -> { m | newKana = kanaModel }) <|
                        Cmd.map KanaMsg kanaCmd
           )


updateKanas : KanasTypes.Msg -> Return Msg Model -> Return Msg Model
updateKanas msg writer =
    let
        ( appModel, _ ) =
            writer

        ( kanasModel, kanasCmd ) =
            KanasState.update msg appModel.kanas
    in
    writer
        --|> (case Debug.log "ts " msg of
        |> (case msg of
                KanasTypes.KanasFetched (Ok kanas) ->
                    Return.map <|
                        \m -> { m | kanas = List.map KanasState.createKanaItem kanas }

                KanasTypes.DeleteKana kanaItem ->
                    Return.mapWith
                        (\m -> { m | kanas = KanasState.deleteKanaItem kanaItem kanasModel })
                        (Cmd.map KanaMsg <|
                            Kana.deleteKana kanaItem.kana
                        )

                KanasTypes.UpdateKana kanaItem ->
                    Return.mapWith
                        (\m -> { m | kanas = kanasModel })
                        (Cmd.map KanaMsg <|
                            Kana.updateKana <|
                                .kana kanaItem
                        )

                KanasTypes.ToggleKanaDone kanaItem ->
                    let
                        kana =
                            kanaItem.kana

                        kana_ =
                            { kana | completed = not kana.completed }
                    in
                    Return.mapWith
                        (\m -> { m | kanas = KanasState.updateKana kana_ kanasModel })
                        (Cmd.map KanaMsg <|
                            Kana.updateKana kana_
                        )

                KanasTypes.SetVisibility visibility ->
                    Return.map (\m -> { m | kanasVisibility = visibility })

                _ ->
                    Return.mapWith
                        (\m -> { m | kanas = kanasModel })
                        (Cmd.map KanasMsg kanasCmd)
           )
