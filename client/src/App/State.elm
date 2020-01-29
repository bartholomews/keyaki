module App.State exposing (initialCommand, initialModel, update, updateEntries, updateEntry)

import App.Types exposing (..)
import Browser
import Browser.Navigation as Nav
import Entries.Api as EntriesApi
import Entries.State as EntriesState
import Entries.Types as EntriesTypes
import Entry.Api as Entry
import Entry.State as EntryState
import Entry.Types as EntryTypes
import Return exposing (Return)
import Url exposing (Url)



-- Model


initialModel : Url -> Nav.Key -> Config -> Model
initialModel url key config =
    { url = url
    , navKey = key
    , config = config
    , entries = EntriesState.initialEntries
    , entriesVisibility = EntriesState.initialVisibility
    , newEntry = EntryState.initialNewEntry
    }



-- Cmd


initialCommand : Config -> Cmd Msg
initialCommand config =
    Cmd.map EntriesMsg (EntriesApi.getEntries config)



-- update


update : Msg -> Model -> Return Msg Model
update msg model =
    Return.singleton model
        |> (case msg of
                EntriesMsg msg_ ->
                    updateEntries msg_

                EntryMsg msg_ ->
                    updateEntry msg_

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


updateEntry : EntryTypes.Msg -> Return Msg Model -> Return Msg Model
updateEntry msg writer =
    let
        ( appModel, _ ) =
            writer

        ( newEntryModel, entryCmd ) =
            EntryState.update msg appModel.newEntry
    in
    writer
        --|> (case Debug.log "t " msg of
        |> (case msg of
                EntryTypes.Save ->
                    Return.command (Cmd.map EntryMsg <| Entry.saveEntry appModel.config newEntryModel)

                EntryTypes.Saved (Ok entry) ->
                    let
                        --entry =
                        --    { newEntryModel | id = entryId }
                        entries =
                            List.append appModel.entries [ EntriesState.createEntryItem entry ]
                    in
                    Return.map
                        (\m -> { m | entries = entries })

                _ ->
                    Return.mapWith (\m -> { m | newEntry = newEntryModel }) <|
                        Cmd.map EntryMsg entryCmd
           )


updateEntries : EntriesTypes.Msg -> Return Msg Model -> Return Msg Model
updateEntries msg writer =
    let
        ( appModel, _ ) =
            writer

        ( entriesModel, entriesCmd ) =
            EntriesState.update msg appModel.entries
    in
    writer
        --|> (case Debug.log "ts " msg of
        |> (case msg of
                EntriesTypes.EntriesFetched (Ok entries) ->
                    Return.map <|
                        \m -> { m | entries = List.map EntriesState.createEntryItem entries }

                EntriesTypes.DeleteEntry entryItem ->
                    Return.mapWith
                        (\m -> { m | entries = EntriesState.deleteEntryItem entryItem entriesModel })
                        (Cmd.map EntryMsg <|
                            Entry.deleteEntry appModel.config entryItem.entry
                        )

                EntriesTypes.UpdateEntry entryItem ->
                    Return.mapWith
                        (\m -> { m | entries = entriesModel })
                        (Cmd.map EntryMsg <|
                            Entry.updateEntry appModel.config <|
                                .entry entryItem
                        )

                EntriesTypes.ToggleEntryDone entryItem ->
                    let
                        entry =
                            entryItem.entry

                        entry_ =
                            { entry | active = not entry.active }
                    in
                    Return.mapWith
                        (\m -> { m | entries = EntriesState.updateEntry entry_ entriesModel })
                        (Cmd.map EntryMsg <|
                            Entry.updateEntry appModel.config entry_
                        )

                EntriesTypes.SetVisibility visibility ->
                    Return.map (\m -> { m | entriesVisibility = visibility })

                _ ->
                    Return.mapWith
                        (\m -> { m | entries = entriesModel })
                        (Cmd.map EntriesMsg entriesCmd)
           )
