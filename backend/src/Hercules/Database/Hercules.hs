{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings #-}

module Hercules.Database.Hercules where

import Data.Aeson
import Data.Aeson.Types
import Data.Text.Encoding              (encodeUtf8) 
import Data.ByteString
import Data.Profunctor
import Data.Profunctor.Product
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import Data.Text
import GHC.Int
import Opaleye                         hiding (fromNullable)

-- | A newtype around @a -> Maybe b@ to facilitate conversions from the
-- Nullable types.
newtype ToMaybe a b = ToMaybe { unToMaybe :: a -> Maybe b }

instance Profunctor ToMaybe where
  dimap f g (ToMaybe h) = ToMaybe (fmap g . h . f)

instance ProductProfunctor ToMaybe where
  empty = ToMaybe pure
  (ToMaybe f) ***! (ToMaybe g) = ToMaybe (\(x, y) -> (,) <$> f x <*> g y)

-- | This instance makes sure that values which are required in the output are
-- required in the input.
instance Default ToMaybe (Maybe a) a where
  def = ToMaybe id

-- | This instance allows values which are optional in the output to be
-- optional in the input.
instance Default ToMaybe (Maybe a) (Maybe a) where
  def = ToMaybe pure

-- | Convert from any Nullable type by "sequencing" over all the fields.
fromNullable :: Default ToMaybe a b => a -> Maybe b
fromNullable = unToMaybe def

---- Types for table: users ----

data User' c1 c2 c3 c4 c5 =
  User
    { userId          :: c1
    , userName        :: c2
    , userEmail       :: c3
    , userGithubId    :: c4
    , userGithubToken :: c5
    }

type User = User' Int64 (Maybe Text) (Maybe Text) (Maybe Text) (Maybe ByteString)

type UserReadColumns = User' (Column PGInt8) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGBytea))

type UserWriteColumns = User' (Maybe (Column PGInt8)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGBytea)))

type UserNullableColumns = User' (Column (Nullable PGInt8)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGBytea))

type UserNullable = User' (Maybe Int64) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe ByteString)

fromNullableUser :: UserNullable -> Maybe User
fromNullableUser = fromNullable

$(makeAdaptorAndInstance "pUser" ''User')

userTable :: Table UserWriteColumns UserReadColumns
userTable = Table "users" (pUser
  User
    { userId = optional "id"
    , userName = optional "name"
    , userEmail = optional "email"
    , userGithubId = optional "github_id"
    , userGithubToken = optional "github_token"
    }
  )

data BitbucketPR' c1 c2 c3 c4 c5 c6 c7 c8 c9 = 
  BitbucketPR 
    { bitbucketPRId         :: c1 
    , bitbucketPRCommit     :: c2
    , bitbucketPRHeadbranch :: c3
    , bitbucketPRBasebranch :: c4
    , bitbucketPRRepo       :: c5 
    , bitbucketPRState      :: c6
    , bitbucketPRTitle      :: c7
    , bitbucketPRCreatedat  :: c8 
    , bitbucketPRUpdatedat  :: c9
    }
   deriving (Show)
type BitbucketPR = BitbucketPR' Int64 ByteString Text Text Text Text Text Text Text    

instance FromJSON BitbucketPR where 
  parseJSON j = do 
           jsonBitbucketPR  <- parseJSON j   
           id            <- (jsonBitbucketPR .: "pullrequest") >>= (.: "id" )                  :: Parser Int64 
           commit        <- (jsonBitbucketPR .: "pullrequest") >>= (.: "source")      >>= (.: "commit" ) >>= (.: "hash" ) :: Parser Text  
           headbranch    <- (jsonBitbucketPR .: "pullrequest") >>= (.: "source")      >>= (.: "branch" ) >>= (.: "name" ) :: Parser Text 
           basebranch    <- (jsonBitbucketPR .: "pullrequest") >>= (.: "destination") >>= (.: "branch" ) >>= (.: "name" ) :: Parser Text 
           repo          <- (jsonBitbucketPR .: "repository")  >>= (.: "full_name" )           :: Parser Text  
           state         <- (jsonBitbucketPR .: "pullrequest") >>= (.: "state" )               :: Parser Text
           title         <- (jsonBitbucketPR .: "pullrequest") >>= (.: "title" )               :: Parser Text
           createdat     <- (jsonBitbucketPR .: "pullrequest") >>= (.: "created_on" )          :: Parser Text
           updatedat     <- (jsonBitbucketPR .: "pullrequest") >>= (.: "updated_on" )          :: Parser Text
           return BitbucketPR { bitbucketPRId         = id          
                              , bitbucketPRCommit     = encodeUtf8 commit     
                              , bitbucketPRHeadbranch = headbranch 
                              , bitbucketPRBasebranch = basebranch 
                              , bitbucketPRRepo       = repo        
                              , bitbucketPRState      = state      
                              , bitbucketPRTitle      = title      
                              , bitbucketPRCreatedat  = createdat   
                              , bitbucketPRUpdatedat  = updatedat  
                              }
        
