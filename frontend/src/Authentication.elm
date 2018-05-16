module Authentication exposing (authenticateFirst)

import Dict exposing (Dict)
import Models exposing (..)
import Urls exposing (..)
import Navigation
import UrlParser exposing (parsePath)

type ParseResult =
    Error String
  | UrlParams (Dict String (Maybe String))


authenticateFirst : Navigation.Location -> (Page, Maybe User)
authenticateFirst location = let
                               queryParams = Debug.log "sup" <| parseParams location.search             
                             in
                               case queryParams of 
                                    UrlParams params -> 
                                            (Maybe.withDefault Home (parsePath pageParser location), updateParams params)
                                    Error err -> 
                                            (Maybe.withDefault Home (parsePath pageParser location), Nothing )


splitAtFirst : Char -> String -> (String, Maybe String)
splitAtFirst c s =
  case firstOccurrence c s of
    Nothing -> (s, Nothing)
    Just i  -> ( String.left i s
               , let right = String.dropLeft (i + 1) s
                 in if right == ""
                    then Nothing
                    else Just right
               )


firstOccurrence : Char -> String -> Maybe Int
firstOccurrence c s =
  case String.indexes (String.fromChar c) s of
    []        -> Nothing
    head :: _ -> Just head

parseSearchString : String -> ParseResult
parseSearchString startsWithQuestionMarkThenParams =
  case String.uncons startsWithQuestionMarkThenParams of
    Nothing -> Error "No URL params"
    Just ('?', rest) -> parseParams rest
    _ -> Error "Doesn't start with `?`"

parseParams : String -> ParseResult
parseParams stringWithAmpersands =
  let
    eachParam = (String.split "&" stringWithAmpersands)
    eachPair  = List.map (splitAtFirst '=') eachParam
  in
    case (List.length <| eachPair) of 
        1 ->
            Error "no queries"
        _ ->
            UrlParams (Dict.fromList eachPair)


updateParams : Dict String (Maybe String )-> Maybe User 
updateParams params = Debug.log "user" <| Just { id = ""
                    , name = "Mohamed Chawki Cheikh"
                    , email = "mohamedchawki.cheikh@predictix.com"
                    , roles = []
                    , recieveEvaluationErrors = False
                    , token = Maybe.withDefault Nothing <| Dict.get "?jwt" params 
                    }

