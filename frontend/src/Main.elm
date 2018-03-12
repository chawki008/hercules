module Main exposing (..)

import Maybe
import Material
import Navigation
import UrlParser exposing (parsePath)
import Hercules exposing (..)
import Msg exposing (..)
import Models exposing (..)
import Update exposing (..)
import View exposing (..)
import Urls exposing (..)
import Http


init : Flags -> Navigation.Location -> ( AppModel, Cmd Msg )
init flags location =
    let
        page = Maybe.withDefault Home (parsePath pageParser location)
        model = initialModel page flags
    in model ! [ Material.init Mdl
               , title (pageToTitle page)
               , Http.send GetProjects  (getProjectWithJobsets "/api")                
               ]


main : Program Flags AppModel Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = Material.subscriptions Mdl
        }
