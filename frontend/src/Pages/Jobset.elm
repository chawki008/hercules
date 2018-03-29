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


view : AppModel -> List (Html Msg)
view model =
    case model.jobsetPage of
        Err _ ->
            [ p [] [ text "TODO" ] ]

        Ok jobset ->
            renderHeader model "Jobset" (Just jobset.name) Nothing
                ++ [ Tabs.render Mdl
                        [ 4 ]
                        model.mdl
                        [ Tabs.ripple
                          --, Tabs.onSelectTab SelectTab
                          --, Tabs.activeTab model.tab
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
                        ]
                        (if List.isEmpty [ 1 ] then
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
                            , Table.table [ Options.css "width" "100%" ]
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
                                                    , Table.td [] [ text evaluation.inputChanges ]
                                                    , Table.td [] (statusLabels evaluation.jobSummary.succeeded evaluation.jobSummary.failed evaluation.jobSummary.inQueue)
                                                    , Table.td [] [ text  evaluation.evaluatedAt ]
                                                    ]
                                            )
                                    )
                                ]
                            ]
                        )
                   ]
