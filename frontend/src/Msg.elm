module Msg exposing (..)

import Material
import Http
import Navigation
import Components.LiveSearch as LiveSearch
import Urls exposing (Page)
import Hercules exposing (Project, ProjectWithJobsets, Jobseteval)


type LoginType
    = Hydra
    | Google


type Msg
    = Mdl (Material.Msg Msg)
    | FetchSucceed String
    | FetchFail Http.Error
    | LoginUserClick LoginType
    | LogoutUserClick
    | PreferencesClick
    | LiveSearchMsg LiveSearch.Msg
    | NewPage Page
    | ClickCreateProject
    | UrlChange Navigation.Location
    | GetProjects (Result Http.Error (List Project))
    | GetProjectWithJobsets (Result Http.Error (ProjectWithJobsets))
    | GetJobsetEvals (Result Http.Error (List Jobseteval))
    