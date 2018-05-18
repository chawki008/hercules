port module Main exposing (..)

import Material
import Navigation
import Msg exposing (..)
import Models exposing (..)
import Update exposing (..)
import View exposing (..)
import Urls exposing (..)
import Task 
import Authentication 
import Actions.Navigation  exposing (title)

init : Flags -> Navigation.Location -> ( AppModel, Cmd Msg )
init flags location =
    let
        (page, mUser) = Authentication.authenticateFirst location
        model = initialModel page flags
    in 
        {model | user = if (mUser /= Nothing) then mUser else model.user} ! [ Material.init Mdl
                                                                             , title (pageToTitle page)
                                                                             , Task.succeed (NewPage page)  |> Task.perform identity  
                                                                             , if (mUser /= Nothing) then setStorage mUser else Cmd.none        
                                                                             ]


main : Program Flags AppModel Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = Material.subscriptions Mdl
        }

port setStorage : Maybe User -> Cmd msg



-- parseLocation : Navigation.Location -> ParseResult
-- parseLocation location = parseParams 
