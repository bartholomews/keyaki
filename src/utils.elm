module Utils exposing (sandbox)

import Browser


sandbox init update view =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
