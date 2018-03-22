module Utils exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Material.Elevation as Elevation
import Material.Button as Button
import Material.Color as Color
import Material.Icon as Icon
import Material.Options as Options
import Msg exposing (..)
import Urls exposing (..)
import Models exposing (..) 
import Hercules as H
import Date


menuIcon : String -> Html Msg
menuIcon name =
    Icon.view name [ Options.css "width" "40px" ]


onPreventDefaultClick : msg -> Attribute msg
onPreventDefaultClick message =
    onWithOptions "click" { defaultOptions | preventDefault = True } (Json.succeed message)


onClickPage : Page -> List (Attribute Msg)
onClickPage page =
    [ style [ ( "pointer", "cursor" ) ]
    , href (pageToURL page)
    , onPreventDefaultClick (NewPage page)
    ]


optionalTag : Bool -> Html Msg -> Html Msg
optionalTag doInclude html =
    if doInclude then
        html
    else
        text ""


statusLabels : Int -> Int -> Int -> List (Html Msg)
statusLabels succeeded failed queued =
    [ optionalTag (succeeded > 0)
        (badge
            (Color.color Color.Green Color.S500)
            [ Options.attribute <| title "Jobs succeeded" ]
            [ text (toString succeeded) ]
        )
    , optionalTag (failed > 0)
        (badge
            (Color.color Color.Red Color.S500)
            [ Options.attribute <| title "Jobs failed" ]
            [ text (toString failed) ]
        )
    , optionalTag (queued > 0)
        (badge
            (Color.color Color.Grey Color.S500)
            [ Options.attribute <| title "Jobs in queue" ]
            [ text (toString queued) ]
        )
    ]


badge : Color.Color -> List (Options.Property c Msg) -> List (Html Msg) -> Html Msg
badge color properties content =
    Options.span
        ([ Options.css "border-radius" "9px"
         , Options.css "padding" "3px 5px"
         , Options.css "line-height" "14px"
         , Options.css "white-space" "nowrap"
         , Options.css "font-weight" "bold"
         , Options.css "font-size" "12px"
         , Options.css "margin" "0 3px"
         , Options.css "cursor" "help"
         , Options.css "color"
            (if color == Color.white then
                "#000"
             else
                "#FFF"
            )
         , Color.background color
         ]
            ++ properties
        )
        content


whiteBadge : List (Options.Property c Msg) -> List (Html Msg) -> Html Msg
whiteBadge properties content =
    badge Color.white properties content


render404 : String -> List (Html Msg)
render404 reason =
    [ Options.div
        [ Elevation.e2
        , Options.css "padding" "40px"
        , Options.center
        ]
        [ text reason ]
    ]


renderHeader : AppModel -> String -> Maybe String -> Maybe Page -> List (Html Msg)
renderHeader model name subname page =
    let
        subnameHtml =
            case subname of
                Nothing ->
                    []

                Just s ->
                    [ small [ style [ ( "margin-left", "10px" ) ] ]
                        [ text s ]
                    ]

        pageHtml =
            case page of
                Nothing ->
                    []

                Just p ->
                    [ Button.render Mdl
                        [ 2 ]
                        model.mdl
                        [ Button.fab
                        , Button.colored
                        , Button.onClick (NewPage p)
                        , Options.css "margin-left" "20px"
                        ]
                        [ Icon.i "add" ]
                    ]
    in
        [ h1
            [ style [ ( "margin-bottom", "30px" ) ] ]
            ([ text name ] ++ subnameHtml ++ pageHtml)
        ]

mapProjectWithJobsets : Maybe H.ProjectWithJobsetsWithStatus -> Project 
mapProjectWithJobsets  maybeProjectWithJobsets = 

                    case maybeProjectWithJobsets of 

                        Just projectWithJobsets -> 
                            { id = projectWithJobsets.project.projectName
                            , name = projectWithJobsets.project.projectDisplayname
                            , description = Maybe.withDefault "" projectWithJobsets.project.projectDescription 
                            , isShown = True
                            , jobsets = List.map mapJobset projectWithJobsets.jobsets
                            }

                        Nothing ->  { id = "no Project exists having this name"
                                    , name = ""
                                    , description =""
                                    , isShown = True
                                    , jobsets = []
                                    }
        

mapProject : H.Project -> Project
mapProject project = 
              { id = project.projectName
              , name = project.projectDisplayname
              , description = Maybe.withDefault "" project.projectDescription 
              , isShown = True
              , jobsets = []
              }


mapJobset : H.JobsetWithStatus -> Jobset
mapJobset jobsetWithStatus = 
    { id = jobsetWithStatus.jobset.jobsetName
    , name = jobsetWithStatus.jobset.jobsetName
    , description = Maybe.withDefault ""  jobsetWithStatus.jobset.jobsetDescription
    , queued =  jobsetWithStatus.jobsetStatus.queued
    , failed =  jobsetWithStatus.jobsetStatus.failed
    , succeeded =  jobsetWithStatus.jobsetStatus.succeeded
    , isShown = True
    , lastEvaluation =  timestampToString (Maybe.withDefault 0 jobsetWithStatus.jobsetStatus.lastevaluatedAt)
    } 
                       
                      
mapJobsetevalsToJobsetPage : List (H.Jobseteval) -> JobsetPage
mapJobsetevalsToJobsetPage jobsetevals = 
    let 
        latestJobsetEval = Maybe.withDefault  { jobsetevalId = 0
                                            , jobsetevalProject = ""
                                            , jobsetevalJobset =  ""
                                            , jobsetevalTimestamp = 0
                                            , jobsetevalCheckouttime = 0
                                            , jobsetevalEvaltime = 0
                                            , jobsetevalHasnewbuilds = 0
                                            , jobsetevalHash = ""
                                            , jobsetevalNrbuilds = Just 0
                                            , jobsetevalNrsucceeded = Just 0
                                            } 
                                            (List.head jobsetevals) 
    in
        { latestCheckTime = "2016-08-06 12:38:01"
        , latestEvaluationTime = timestampToString latestJobsetEval.jobsetevalTimestamp
        , latestFinishedEvaluationTime = "2016-08-06 12:38:01"
        , evaluations = List.map mapJobsetevalToEval jobsetevals
        , name = latestJobsetEval.jobsetevalJobset
        }

mapJobsetevalToEval : H.Jobseteval -> Evaluation
mapJobsetevalToEval jobseteval =  
    { id = jobseteval.jobsetevalId
    , inputChanges = "dsds"
    , jobSummary = { succeeded = Maybe.withDefault 0 jobseteval.jobsetevalNrsucceeded 
                , failed = (Maybe.withDefault 0 jobseteval.jobsetevalNrbuilds) - (Maybe.withDefault 0 jobseteval.jobsetevalNrsucceeded) 
                , inQueue  =  Maybe.withDefault 0 jobseteval.jobsetevalNrsucceeded 
                }
    , evaluatedAt = timestampToString jobseteval.jobsetevalTimestamp
    }  

timestampToString : Int -> String         
timestampToString timstamp = 
    let 
        evaluatedAt = Date.fromTime (toFloat (timstamp*1000))
        year = toString (Date.year evaluatedAt)
        month = toString (Date.month evaluatedAt)
        day = toString (Date.day evaluatedAt)
        hours = toString (Date.hour evaluatedAt)
        minutes = toString (Date.minute evaluatedAt)
        seconds  = toString (Date.second  evaluatedAt)
    in 
        year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hours ++ ":" ++ minutes ++ ":" ++ seconds