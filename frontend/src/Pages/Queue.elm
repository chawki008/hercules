module Pages.Queue exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models exposing (..)
import Msg exposing (Msg)
import Utils exposing (..)
import Material.Table as Table
import Material.Options as Options
import Urls exposing (..)

queueView : AppModel -> List (Html Msg)
queueView model =
 renderHeader model "Queue Summary" Nothing Nothing 
 ++     [Table.table 
            [Options.css "width" "50%"
            , Options.css "border" "none"
            ]
                [ Table.thead []
                    [ tr []
                        [ th [  ] [ text "Jobset" ]
                        , th [  ] [ text "Queued" ]
                        ]
                    ]
                , Table.tbody []
                     (model.queueSummary.queueSummaryJobsets
                        |>  List.map 
                        (\jobsetSummary ->
                                tr []
                                [ td [] [ a [href (Urls.pageToURL <| Urls.Jobset jobsetSummary.jobsetSummaryProject jobsetSummary.jobsetSummaryJobset )] [text jobsetSummary.jobsetSummaryJobset] ]
                                , td [  ] [ text <| toString jobsetSummary.jobsetSummaryQueued ]
                                ]
                        )
                    )
                ]
            ]

    
