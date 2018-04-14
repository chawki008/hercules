module Hercules.Helpers.Helpers where

import Data.List                            (elemIndex)
import Data.Foldable                        (toList)
import Data.Int                              (Int32, Int64)
import qualified Data.Text                             as T
import qualified Data.List.NonEmpty as NE
import Data.Maybe                           (catMaybes, fromMaybe)
import Hercules.Database.Extra       (JobsetNullable, Project, JobsetevalinputNullable, fromNullableJobsetevalinput
                                      , JobsetStatus (..), Jobset, Jobseteval, Jobsetevalinput
                                      , JobsetWithStatus (..), ProjectWithJobsetsWithStatus (..), JobsetevalWithStatus(..)
                                      , fromNullableJobset, jobsetevalinputName, jobsetevalinputRevision, jobsetevalinputUri
                                      , jobsetevalinputDependency, jobsetevalinputType )


toMaybe :: Maybe a -> Maybe (Maybe a)
toMaybe (Just a) = Just (Just a) 
toMaybe Nothing = Nothing 

dupPrev :: Eq a => [a] -> [(a, Maybe a)]
dupPrev l = fmap (getWithPrev l) l 

getWithPrev :: Eq a => [a] -> a -> (a, Maybe a)
getWithPrev l element = let 
                            index = fromMaybe 0 $ elemIndex element l 
                        in
                          if index < (length l - 1) then (element, Just (l !!  (index + 1)))
                                        else (element, Nothing)

                                                                  
fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b

thrd4 :: (a,b,c,d) -> c
thrd4 (_,_,c,_) = c

frth4 :: (a,b,c,d) -> d
frth4 (_,_,_,d) = d

fst6 :: (a,b,c,d,e,f) -> a
fst6 (a,_,_,_,_,_) = a