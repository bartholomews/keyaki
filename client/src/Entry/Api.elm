module Entry.Api exposing
    ( deleteEntry
    , entryDecoder
    , saveEntry
    , updateEntry
    )

import Common.Encoding exposing (encodeMaybe)
import Entry.Types exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


entryRequestEncoded : EntryRequest -> Encode.Value
entryRequestEncoded entryRequest =
    let
        list =
            [ ( "romaji", Encode.string entryRequest.romaji ), ( "kana", encodeMaybe entryRequest.kana Encode.string ) ]

        -- FIXME: add `meaning: String`
    in
    list |> Encode.object


entryEncoded : Entry -> Encode.Value
entryEncoded entry =
    let
        list =
            [ ( "active", Encode.bool entry.active )
            , ( "romaji", Encode.string entry.kana )
            ]
    in
    list |> Encode.object


entryDecoder : Decoder Entry
entryDecoder =
    Decode.succeed Entry
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "active" Decode.bool
        |> Pipeline.required "romaji" Decode.string


saveEntry : EntryRequest -> Cmd Msg
saveEntry entryRequest =
    let
        body =
            entryRequestEncoded entryRequest
                |> Encode.encode 0
                -- FIXME ???
                |> Http.stringBody "application/json"
    in
    Http.post
        { url = "http://localhost:8081/api/entry/"
        , body = body
        , expect = Http.expectJson Saved entryDecoder
        }


deleteEntry : Entry -> Cmd Msg
deleteEntry entry =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:8081/api/entry/" ++ String.fromInt entry.id
        , body = Http.emptyBody
        , expect = Http.expectString Deleted
        , timeout = Nothing
        , tracker = Nothing

        --        , withCredentials = False
        }


updateEntry : Entry -> Cmd Msg
updateEntry entry =
    let
        body =
            entryEncoded entry
                |> Encode.encode 0
                |> Http.stringBody "application/json"
    in
    Http.request
        { method = "PUT"
        , headers = []
        , url = "http://localhost:8081/api/entry/" ++ String.fromInt entry.id
        , body = body
        , expect = Http.expectString Updated
        , timeout = Nothing
        , tracker = Nothing

        --        , withCredentials = False
        }
