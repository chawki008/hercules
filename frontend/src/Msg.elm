module Msg exposing (..)

import Material
import Http
import Navigation
import Components.LiveSearch as LiveSearch
import Urls exposing (Page)
import Hercules exposing (Project, ProjectWithJobsetsWithStatus
                         , JobsetevalWithStatus, JobsetevalWithBuilds, QueueSummary)


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
    | GetProjectWithJobsets (Result Http.Error (Maybe ProjectWithJobsetsWithStatus))
    | GetJobsetEvals (Result Http.Error (List JobsetevalWithStatus))
    | GetQueueSummary (Result Http.Error QueueSummary)
    | SelectTab Int
    | GetJobsInfo (Result Http.Error (List JobsetevalWithBuilds))
    | SwitchToggle Page Int 
    | UpdateNewProject String String
    | AddProjectSubmit 
    | AddProject (Result Http.Error (String))
    | Test String
    | UpdateNewJobset String String
    | UpdateNewJobsetInput String Int String
    | AddJobsetInput
    | AddJobsetSubmit 
    | AddJobset (Result Http.Error (String))
    | SelectLayoutTab Int