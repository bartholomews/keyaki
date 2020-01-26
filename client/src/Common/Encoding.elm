module Common.Encoding exposing (..)

import Json.Encode as Encode


encodeMaybe : Maybe a -> (a -> Encode.Value) -> Encode.Value
encodeMaybe maybe aEncoder =
    case maybe of
        Just a ->
            aEncoder a

        Nothing ->
            Encode.null
