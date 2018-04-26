module Models exposing (..)

import Material
import Maybe
import Date
import Urls exposing (..)
import Hercules as H
import Array

type alias Flags =
    { backendURL : String
    }

type AlertType
    = Danger
    | Info
    | Warning
    | Success


type alias Alert =
    { kind : AlertType
    , msg : String
    }


type alias User =
    { id : String
    , name : String
    , email : String
    , roles : List String
    , recieveEvaluationErrors : Bool
    }


type alias Jobset =
    { id : String
    , name : String
    , description : String
    , queued : Int
    , failed : Int
    , succeeded : Int
    , lastEvaluation : String
    , isShown : Bool
    }


type alias Project =
    { id : String
    , name : String
    , description : String
    , jobsets : List Jobset
    , isShown : Bool
    , isEnabled : Bool
    , owner : String 
    , url : String
    }


type alias HydraConfig =
    { logo : String
    , hydraVersion : String
    , nixVersion : String
    }


type alias QueueStats =
    { numBuilding : Int
    , numWaiting : Int
    , numMachines : Int
    }


type alias JobSummary =
    { succeeded : Int
    , failed : Int
    , inQueue : Int
    }


type alias Evaluation =
    { id : Int
    , inputChanges : String
    , jobSummary : JobSummary
    , evaluatedAt : String
    }

type alias Job = 
    { name : String
-- (timestamp, status, finished)
    , infos : List (Int, Int, Int) 
    }
type alias JobsetPage =
    { latestCheckTime : String
    , latestEvaluationTime : String
    , latestFinishedEvaluationTime : String
    , evaluations : List Evaluation
    , jobs : List Job
    , name : String
    , project : String
    , selectedTab : Int 
    }


type AjaxError msg
    = AjaxFail msg
    | Loading

type alias NewProjectPage = 
    { project : Maybe Project
    , toggles : Array.Array Bool
    }

type alias NewJobsetPage = 
    { jobset : Maybe JobsetWithInputs 
    , toggles : Array.Array Bool
    , project : String
    , jobsetInputsNr : Int
    }

type alias JobsetWithInputs = 
    { name: String
    , enabled: Int
    , hidden: Bool
    , description: String
    , nixexprinput: String
    , nixexprpath: String
    , checkinterval: Int
    , schedulingshares: Int
    , enableemail: Bool
    , emailoverride: String
    , keepnr: Int
    , inputs : Array.Array JobsetInput
    }   

type alias JobsetInput =
    { inputname : String
    , inputType : String
    , value : String
    , emailResponsible : Bool
    }    
    
type alias AppModel =
    { alert : Maybe Alert
    , hydraConfig : HydraConfig
    , projects : List Project
    , jobsets : Result (AjaxError String) (List Jobset)
    , jobsetPage : Result (AjaxError String) JobsetPage
    , user : Maybe User
    , mdl : Material.Model
    , queueStats : QueueStats
    , searchString : String
    , backendURL : String
    , currentPage : Page
    , newProjectPage : NewProjectPage
    }


initialModel : Page -> Flags -> AppModel
initialModel page flags =
    let
        jobsets =
            [ { id = "release-16.03"
              , name = "release-16.03"
              , description = "NixOS 16.03 release branch"
              , queued = 5
              , failed = 275
              , succeeded = 24315
              , lastEvaluation = "2016-05-21 13:57:13"
              , isShown = True
              }
            , { id = "trunk-combined"
              , name = "trunk-combined"
              , description = "Combined NixOS/Nixpkgs unstable"
              , queued = 1
              , failed = 406
              , succeeded = 24243
              , lastEvaluation = "2016-05-21 13:57:03"
              , isShown = True
              }
            ]
    in
        { alert = Nothing
        , user = Just { id = "chawki12121"
                      , name = "chawki"
                      , email = "chawki@chawki.com"
                      , roles = ["Admin"]
                      , recieveEvaluationErrors = False
                      }
        , backendURL = flags.backendURL
        , mdl = Material.model
        , currentPage = page
        , searchString = ""
        , hydraConfig =
            -- TODO: downsize logo, serve it with webpack
            { logo = "http://nixos.org/logo/nixos-logo-only-hires.png"
            , hydraVersion = "0.1.1234.abcdef"
            , nixVersion = "1.12pre1234_abcdef"
            }
        , queueStats =
            QueueStats 124 32345 19
            -- Pages
        , jobsetPage =  Err (AjaxFail "no jobset yet") 
        , jobsets = Ok []
        , projects =
            [ { id = "nixos"
              , name = "NixOS"
              , description = "the purely functional Linux distribution"
              , isShown = True
              , isEnabled = True
              , jobsets = jobsets
              , owner = ""
              , url = ""
              }
            , { id = "nix"
              , name = "Nix"
              , description = "the purely functional package manager"
              , isShown = True
              , isEnabled = True
              , owner = ""
              , url = ""
              , jobsets =
                    [ { id = "master"
                      , name = "master"
                      , description = "Master branch"
                      , queued = 0
                      , failed = 33
                      , succeeded = 1
                      , isShown = True
                      , lastEvaluation = "2016-05-21 13:57:13"
                      }
                    ]
              }
            , { id = "nixpkgs"
              , name = "Nixpkgs"
              , description = "Nix Packages collection"
              , isShown = True
              , isEnabled = True 
              , owner = ""
              , url = ""
              , jobsets =
                    [ { id = "trunk"
                      , name = "trunk"
                      , description = "Trunk"
                      , isShown = True
                      , queued = 0
                      , failed = 7798
                      , succeeded = 24006
                      , lastEvaluation = "2016-05-21 13:57:13"
                      }
                    , { id = "staging"
                      , name = "staging"
                      , description = "Staging"
                      , isShown = True
                      , queued = 0
                      , failed = 31604
                      , succeeded = 63
                      , lastEvaluation = "2016-05-21 13:57:03"
                      }
                    ]
              }
            , { id = "nixops"
              , name = "NixOps"
              , description = "Deploying NixOS machines"
              , isShown = True
              , isEnabled = True
              , jobsets = []
              , owner = ""
              , url = ""
              }
            ]
            , newProjectPage = { project = Nothing
                               , toggles = Array.fromList [ False, False ]  
                               }
            , newJobsetPage = { jobset = Nothing 
                              , toggles = Array.fromList [ True, False, False, False, False ]
                              , project = ""  
                              , jobsetInputsNr = 0
                              }
        }
