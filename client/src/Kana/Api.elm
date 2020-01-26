module Kana.Api exposing
    ( deleteKana
    , saveKana
    , updateKana
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Kana.Types exposing (..)


kanaEncoded : Kana -> Encode.Value
kanaEncoded kana =
    let
        list =
            [ ( "completed", Encode.bool kana.completed )
            , ( "description", Encode.string kana.description )
            ]
    in
    list |> Encode.object


saveKana : Kana -> Cmd Msg
saveKana kana =
    let
        body =
            kanaEncoded kana
                |> Encode.encode 0
                |> Http.stringBody "application/json"
    in
    Http.post
        { url = "http://localhost:8081/api/kana/"
        , body = body
        , expect = Http.expectJson Saved Decode.int
        }


deleteKana : Kana -> Cmd Msg
deleteKana kana =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:8081/api/kana/" ++ String.fromInt kana.id
        , body = Http.emptyBody
        , expect = Http.expectString Deleted
        , timeout = Nothing
        , tracker = Nothing

        --        , withCredentials = False
        }


updateKana : Kana -> Cmd Msg
updateKana kana =
    let
        body =
            kanaEncoded kana
                |> Encode.encode 0
                |> Http.stringBody "application/json"
    in
    Http.request
        { method = "PUT"
        , headers = []
        , url = "http://localhost:8081/api/kana/" ++ String.fromInt kana.id
        , body = body
        , expect = Http.expectString Updated
        , timeout = Nothing
        , tracker = Nothing

        --        , withCredentials = False
        }
