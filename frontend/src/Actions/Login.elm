module Actions.Login exposing (..)

import Models exposing (..)
import Msg exposing (..)
import Navigation
import Utils as U
import Tuple exposing (first, second)
import String exposing (slice)

loginUserClick : AppModel -> LoginType -> (AppModel, Cmd Msg)
loginUserClick model loginType = 
            let
                user =
                    { id = "domenkozar"
                    , name = "Domen Kožar"
                    , email = "domen@dev.si"
                    , roles = []
                    , recieveEvaluationErrors = False
                    , token = Nothing 
                    }
            in
                case loginType of
                    Hydra ->
                        ( { model | user = Just user }, Cmd.none )

                    Google ->
                        model ! [ Navigation.load <| googleAuthUrl U.googleAuthorizationEndpoint U.googleQueryParams]

googleAuthUrl : String -> List (String, String) -> String 
googleAuthUrl endpoint queryParams =  concat endpoint <| slice 1 -1 <| List.foldl (concat << (concat "&")) "&"  <| List.map queryToString queryParams
                        
queryToString : (String, String) -> String 
queryToString query = first query ++ "=" ++ second query

concat : String -> String -> String 
concat a b = a ++ b
