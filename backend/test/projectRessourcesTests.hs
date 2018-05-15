{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import           Hercules.Lib           (getAppForTest)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (getAppForTest "/home/medchawkicheikh/nix/ci/hercules/chawki008Hercules/hercules/backend/example-config.yaml") $ do
    describe "GET docs" $ do
        it "/docs responds with 200" $ do
            get "/docs" `shouldRespondWith` 200
    describe "GET projectNames" $ do 
        it "/projectNames responds with json200" $ do
            get "/projectNames" `shouldRespondWith` 200 {matchHeaders = [hContentType <:> "application/json"] } 
    describe "GET projects" $ do 
        it "/projects responds with json200" $ do
            get "/projects" `shouldRespondWith` 200 {matchHeaders = [hContentType <:> "application/json"] } 
    describe "GET project" $ do     
        it "/projects/FAROI responds with json200" $ do
            get "/projects/FAROI" `shouldRespondWith` 200 {matchHeaders = [hContentType <:> "application/json"] } 
    describe "GET projects With Jobsets" $ do 
        it "/projectsWithJobsets responds with json200" $ do
            get "/projectsWithJobsets" `shouldRespondWith` 200 {matchHeaders = [hContentType <:> "application/json"] } 
    describe "GET project With Jobsets" $ do 
        it "/projectsWithJobsets/FAROI responds with json200" $ do
            get "/projectsWithJobsets/FAROI" `shouldRespondWith` 200 {matchHeaders = [hContentType <:> "application/json"] } 
    describe "GET all jobsets evals" $ do 
        it "/projects/FAROI/demand-mgmt_pr_373/jobsetevals responds with json200" $ do
            get "/projects/FAROI/demand-mgmt_pr_373/jobsetevals" `shouldRespondWith` 200 {matchHeaders = [hContentType <:> "application/json"] } 
    describe "GET all jobsets evals with builds" $ do 
        it "/projects/FAROI/demand-mgmt_pr_373/jobs responds with json200" $ do
            get "/projects/FAROI/demand-mgmt_pr_373/jobs" `shouldRespondWith` 200 {matchHeaders = [hContentType <:> "application/json"] } 
            