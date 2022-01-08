module Entry.Api exposing
    ( deleteEntry
    , entryDecoder
    , saveEntry
    , updateEntry
    )

import App.Types exposing (Config)
import Common.Encoding exposing (encodeMaybe)
import Common.Url exposing (appendPath)
import Entry.Types exposing (..)
import Http exposing (jsonBody)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Url exposing (Url)


entryRequestEncoded : EntryRequest -> Encode.Value
entryRequestEncoded entryRequest =
    let
        list =
            [ ( "kana", encodeMaybe entryRequest.kana Encode.string ), ( "meaning", Encode.string entryRequest.meaning ) ]
    in
    list |> Encode.object


entryEncoded : Entry -> Encode.Value
entryEncoded entry =
    let
        list =
            [ ( "active", Encode.bool entry.active )
            , ( "kana", Encode.string entry.kana )
            , ( "meaning", Encode.string entry.meaning )
            ]
    in
    list |> Encode.object


entryDecoder : Decoder Entry
entryDecoder =
    Decode.succeed Entry
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "active" Decode.bool
        |> Pipeline.required "kana" Decode.string
        |> Pipeline.required "meaning" Decode.string


saveEntry : Config -> EntryRequest -> Cmd Msg
saveEntry config entryRequest =
    let
        body =
            entryRequestEncoded entryRequest
                |> Encode.encode 0
                -- FIXME ???
                |> Http.stringBody "application/json"
    in
    Http.post
        { url = Url.toString (appendPath config.serverApi "entries")
        , body = body
        , expect = Http.expectJson Saved entryDecoder
        }


deleteEntry : Config -> Entry -> Cmd Msg
deleteEntry config entry =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = Url.toString (appendPath config.serverApi ("entries/" ++ String.fromInt entry.id))
        , body = Http.emptyBody
        , expect = Http.expectString Deleted
        , timeout = Nothing
        , tracker = Nothing

        --        , withCredentials = False
        }


updateEntry : Config -> Entry -> Cmd Msg
updateEntry config entry =
    let
        body =
            entryEncoded entry
                |> jsonBody
    in
    Http.request
        { method = "PUT"
        , headers = []
        , url = Url.toString (appendPath config.serverApi ("entries/" ++ String.fromInt entry.id))
        , body = body
        , expect = Http.expectString Updated
        , timeout = Nothing
        , tracker = Nothing

        --        , withCredentials = False
        }
