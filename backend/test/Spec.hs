{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import           Hercules.Lib           (getAppForTest)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Method
import           Hercules.Database.Extra 
import           Data.String   
import qualified Data.ByteString.Lazy as BS


main :: IO ()
main = hspec spec

spec :: Spec
spec = with (getAppForTest "example-config.yaml") $ do
    describe "GET docs" $ do
        it "/docs responds with 200" $ do
            get "/docs" `shouldRespondWith` 200
--     describe "GET projectNames" $ do 
--         it "/projectNames responds with json200" $ do
--             get "/projectNames" `shouldRespondWith` 200 {matchHeaders = [hContentType <:> "application/json"] } 
--     describe "GET projects" $ do 
--         it "/projects responds with json200" $ do
--             get "/projects" `shouldRespondWith` 200 {matchHeaders = [hContentType <:> "application/json"] } 
--     describe "GET project" $ do     
--         it "/projects/projectName responds with json200" $ do
--             get "/projects/projectName" `shouldRespondWith` testProject 
--     describe "GET projects With Jobsets" $ do 
--         it "/projectsWithJobsets responds with json200" $ do
--             get "/projectsWithJobsets" `shouldRespondWith` 200 {matchHeaders = [hContentType <:> "application/json"] } 
--     describe "GET project With Jobsets" $ do 
--         it "/projectsWithJobsets/projectName1 responds with json200" $ do
--             get "/projectsWithJobsets/projectName1" `shouldRespondWith` testProjectWithJobsets 
--     describe "GET all jobsets evals" $ do 
--         it "/projects/projectName/jobsetName1/jobsetevals responds with json200" $ do
--             get "/projects/projectName/jobsetName1/jobsetevals" `shouldRespondWith` emptyList
--     describe "GET all jobsets evals with builds" $ do 
--         it "/projects/projectName/jobsetName1/jobs responds with json200" $ do
--             get "/projects/projectName/jobsetName1/jobs" `shouldRespondWith` emptyList
--     describe "Post already Existing project" $ do 
--         it "/projects responds " $ do
--             request methodPost "/projects" [(hContentType, "application/json")] alreadyExistingProject `shouldRespondWith` "project already exists" {matchStatus = 409} 
--     describe "Post jobset to non existing project" $ do 
--         it "/projects/nonExistingProjectName responds " $ do
--             request methodPost "/projects/nonExistingProjectName" [(hContentType, "application/json")] testJobsetWithInputs `shouldRespondWith` "project doesn't exist" {matchStatus = 404}
--     describe "Post jobset to an existing jobset" $ do 
--         it "/projects/projectName responds " $ do
--             request methodPost "/projects/projectName" [(hContentType, "application/json")] testJobsetWithInputs `shouldRespondWith` "jobset already exists" {matchStatus = 409}
     
-- testProject :: ResponseMatcher 
-- testProject = fromString "{\"projectDisplayname\":\"projectDisplayname\",\"projectHomepage\":\"projectHomepage\",\"projectOwner\":\"projectOwner\",\"projectHidden\":1,\"projectName\":\"projectName\",\"projectRepo\":null,\"projectEnabled\":1,\"projectDescription\":\"projectDescription\"}"

-- emptyList :: ResponseMatcher 
-- emptyList = fromString "[]"

-- testProjectWithJobsets :: ResponseMatcher
-- testProjectWithJobsets = fromString "{\"project\":{\"projectDisplayname\":\"projectDisplayname\",\"projectHomepage\":\"projectHomepage\",\"projectOwner\":\"projectOwner\",\"projectHidden\":1,\"projectName\":\"projectName1\",\"projectRepo\":null,\"projectEnabled\":1,\"projectDescription\":null},\"jobsets\":[]}"

-- alreadyExistingProject :: BS.ByteString 
-- alreadyExistingProject = fromString "{\"projectName\":\"projectName1\",\"projectDisplayname\":\"projectDisplayname\",\"projectEnabled\":1,\"projectHidden\":1,\"projectOwner\":\"projectOwner\",\"projectRepo\":\"projectRepo\",\"projectHomepage\":\"projectHomepage\"}"

-- testJobsetWithInputs :: BS.ByteString
-- testJobsetWithInputs = fromString "{\"name\":\"jobsetName1\",\"enabled\":1,\"hidden\":false,\"description\":\"Jobsets\",\"nixexprinput\":\"src\",\"nixexprpath\":\"default.nix\",\"checkinterval\":300,\"schedulingshares\":100,\"enableemail\":false,\"emailoverride\":\"\",\"keepnr\":3,\"inputs\":{\"src\":{\"type\":\"git\",\"value\":\"git:/github.com/shlevy/declarative-hydra-example.git\",\"emailresponsible\":false},\"nixpkgs\":{\"type\":\"git\",\"value\":\"git:/github.com/NixOS/nixpkgs.gitrelease-16.03\",\"emailresponsible\":false}}}"
