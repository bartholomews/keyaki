module App.Types exposing (LinkName(..), Model, Msg(..), RouteLink)

import Browser
import Browser.Navigation as Nav
import Kana.Types as Kana
import Kanas.Types as Kanas
import Url exposing (Url)


type alias Model =
    { url : Url
    , navKey : Nav.Key
    , kanas : Kanas.Kanas
    , kanasVisibility : Kanas.Visibility
    , newKana : Kana.Kana
    }


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | KanasMsg Kanas.Msg
    | KanaMsg Kana.Msg
    | DoNothing


type LinkName
    = HOME
    | SRS


type alias RouteLink =
    { urlPath : String
    , linkName : LinkName
    }
