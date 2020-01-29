module Common.Url exposing (..)

import Common.String exposing (ltrim)
import Url exposing (Url)


appendPath : Url -> String -> Url
appendPath url newPath =
    let
        path =
            if String.endsWith "/" url.path then
                url.path ++ ltrim "/" newPath

            else
                url.path ++ "/" ++ ltrim "/" newPath
    in
    { url | path = path }
