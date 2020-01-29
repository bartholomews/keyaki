module Common.String exposing (..)

import String exposing (dropLeft, length, startsWith)


ltrim : String -> String -> String
ltrim toRemove str =
    if startsWith toRemove str then
        dropLeft (length toRemove) str

    else
        str
