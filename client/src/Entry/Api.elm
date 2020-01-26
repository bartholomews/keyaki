module Entry.Api exposing
    ( deleteEntry
    , saveEntry
    , updateEntry
    )

import Entry.Types exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


entryEncoded : Entry -> Encode.Value
entryEncoded entry =
    let
        list =
            [ ( "active", Encode.bool entry.active )
            , ( "romanji", Encode.string entry.romanji )
            ]
    in
    list |> Encode.object


saveEntry : Entry -> Cmd Msg
saveEntry entry =
    let
        body =
            entryEncoded entry
                |> Encode.encode 0
                |> Http.stringBody "application/json"
    in
    Http.post
        { url = "http://localhost:8081/api/entry/"
        , body = body
        , expect = Http.expectJson Saved Decode.int
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
