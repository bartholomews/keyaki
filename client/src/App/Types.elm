module App.Types exposing (LinkName(..), Model, Msg(..), RouteLink)

import Browser
import Browser.Navigation as Nav
import Entries.Types as Entries
import Entry.Types as Entry
import Url exposing (Url)


type alias Model =
    { url : Url
    , navKey : Nav.Key
    , entries : Entries.Entries
    , entriesVisibility : Entries.Visibility
    , newEntry : Entry.Entry
    }


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | EntriesMsg Entries.Msg
    | EntryMsg Entry.Msg
    | DoNothing


type LinkName
    = HOME
    | SRS


type alias RouteLink =
    { urlPath : String
    , linkName : LinkName
    }
