module Actions.Jobseteval exposing(..)

import Utils as U
import Models exposing (..)
import Msg exposing (..)
import Hercules as H



-- #########################################################--
getJobsetEvals : AppModel -> List H.JobsetevalWithStatus -> (AppModel, Cmd Msg) 
getJobsetEvals  model evalList = 
        ({ model | jobsetPage = Ok (U.mapJobsetevalsToJobsetPage evalList) }, Cmd.none )

-- #########################################################--
getJobsInfo : AppModel -> List H.JobsetevalWithBuilds -> (AppModel, Cmd Msg)
getJobsInfo model jobsInfo = 
    let 
        okOldJobsetPage = model.jobsetPage 
        newJobsetPage = updateJobsetPage okOldJobsetPage jobsInfo

    in
        ({model | jobsetPage = newJobsetPage }, Cmd.none )

updateJobsetPage : Result (AjaxError String) JobsetPage -> List (H.JobsetevalWithBuilds) -> Result (AjaxError String) JobsetPage
updateJobsetPage okOldJobsetPage jobs = case okOldJobsetPage of
                                        Ok oldJobsetPage -> 
                                            let 
                                                oldJobsetPage1 = { oldJobsetPage | selectedTab = 1}
                                            in
                                                Ok {oldJobsetPage1 | jobs = U.mapToJobs jobs}
                                        _ -> 
                                             Err (AjaxFail "no jobset yet") 
