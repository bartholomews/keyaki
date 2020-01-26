module App.Types exposing (HeaderLink, HeaderLinkName(..), Model, Msg(..))

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
    , newEntry : Entry.EntryRequest
    }


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | EntriesMsg Entries.Msg
    | EntryMsg Entry.Msg
    | DoNothing


type HeaderLinkName
    = HOME
    | SRS


type alias HeaderLink =
    { urlPath : String
    , name : HeaderLinkName
    }
