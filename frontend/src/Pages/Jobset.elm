module Pages.Jobset exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Material.List as List
import Material.Options as Options
import Material.Tabs as Tabs
import Material.Table as Table
import Models exposing (..)
import Msg exposing (..)
import Utils exposing (..)
import Urls as Urls exposing (..)
import String exposing(slice)
import Material.Icon as Icon

view : AppModel -> List (Html Msg)
view model =
    case model.jobsetPage of
        Err _ ->
            [ p [] [ text "TODO" ] ]

        Ok jobset -> renderHeader model "Jobset" (Just jobset.name) Nothing
                ++ [ Tabs.render Mdl
                        [ 4 ]
                        model.mdl
                        [ Tabs.ripple
                          , Tabs.onSelectTab SelectTab
                          , Tabs.activeTab jobset.selectedTab
                        ]
                        [ Tabs.label
                            [ Options.center ]
                            [ Options.span [ Options.css "width" "4px" ] []
                            , text "Evaluations"
                            ]
                        , Tabs.label
                            [ Options.center ]
                            [ Options.span [ Options.css "width" "4px" ] []
                            , text "Jobs"
                            ]
                        , Tabs.label
                            [ Options.center ]
                            [ Options.span [ Options.css "width" "4px" ] []
                            , text "Channels"
                            ]
                        ] (renderSelectedTab jobset)]

renderSelectedTab : JobsetPage -> List (Html Msg)
renderSelectedTab  jobset = 
    case jobset.selectedTab of 
        1 -> jobsTab jobset
        2 -> [ p [] [ text "TODO" ] ]
        _ -> evaluationsTab  jobset
            
        
evaluationsTab :  JobsetPage -> List (Html Msg)
evaluationsTab  jobset = 
                         if List.isEmpty [ 1 ] then
                            render404 "No evaluations yet."
                         else
                            [ List.ul []
                                [ List.li [ List.withSubtitle ]
                                    [ List.content []
                                        [ text "Lastest check"
                                        , List.subtitle [] [ text jobset.latestCheckTime ]
                                        ]
                                    ]
                                , List.li [ List.withSubtitle ]
                                    [ List.content []
                                        [ text "Lastest evaluation"
                                        , List.subtitle []
                                            [ a
                                                [ href "TODO" ]
                                                [ text jobset.latestEvaluationTime ]
                                            ]
                                        ]
                                    ]
                                , List.li [ List.withSubtitle ]
                                    [ List.content []
                                        [ text "Lastest finished evaluation"
                                        , List.subtitle []
                                            [ a
                                                [ href "TODO" ]
                                                [ text jobset.latestFinishedEvaluationTime ]
                                            ]
                                        ]
                                    ]
                                ]
                            , Table.table [Options.css "width" "100%"]
                                [ Table.thead []
                                    [ Table.tr []
                                        [ Table.th [] [ text "#" ]
                                        , Table.th [] [ text "Input changes" ]
                                        , Table.th [] [ text "Job status" ]
                                        , Table.th [] [ text "Time" ]
                                        ]
                                    ]
                                , Table.tbody []
                                    (jobset.evaluations
                                        |> List.map
                                            (\evaluation ->
                                                Table.tr []
                                                    [ Table.td []
                                                        [ a
                                                            (onClickPage (Urls.Jobset "123" "foo"))
                                                            [ text (toString evaluation.id) ]
                                                        ]
                                                    , Table.td [Options.css  "white-space" "normal"] [ text evaluation.inputChanges ]
                                                    , Table.td [] (statusLabels evaluation.jobSummary.succeeded evaluation.jobSummary.failed evaluation.jobSummary.inQueue)
                                                    , Table.td [] [ text  evaluation.evaluatedAt ]
                                                    ]
                                            )
                                    )
                                ]
                            ]

jobsTab : JobsetPage -> List (Html Msg)
jobsTab jobset = 
                if List.isEmpty [ 1 ] then
                render404 "No Jobs yet."
                else
                [ 
                 Table.table [ Options.css "width" "100%"]
                    [ Table.thead []
                        [ Table.tr []
                            ([Table.th [] [ text "Job"]]
                              ++ ((List.take 10 jobset.evaluations)
                                        |> List.map 
                                            (\evaluation -> 
                                                Table.th [Options.css "padding" "0 0px 8px 0px"] [ text (slice 0 11 evaluation.evaluatedAt)]
                                            )))
                        ]
                    , Table.tbody []
                        (jobset.jobs 
                            |> List.map 
                                (\job -> 
                                    Table.tr []
                                         ([Table.td [] [text job.name]]
                                         ++ (job.infos
                                                |> List.map 
                                                    (\info ->
                                                        renderStatus info 
                                                    )))
                                
                                )
                        )
                    ]
                ]

renderStatus : (Int, Int, Int) -> (Html Msg)
renderStatus info = 
        case info of 
            (_,_,0) -> 
                Table.td [Options.css "color" "grey"] [  (Icon.view "help" [Icon.size24]) ]
            (_,0,_) -> 
                Table.td [Options.css "color" "green"] [  (Icon.view "checkboxmarkedcircle" [Icon.size18, Options.css "width" "1px"]) ]
            (_,_,_) -> 
                 Table.td [Options.css "color" "red"] [  (Icon.view "error" [Icon.size24]) ]
                                                                                                                                                        
                   