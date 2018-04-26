{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main
  ( main
  ) where

import Data.Text (Text, replace, pack)
import Data.Monoid ((<>))
import Elm
import Servant.Auth.Server
import Servant.Elm
import Servant.Foreign
import Servant.Foreign.Internal (Elem)
import Options.Applicative

import Hercules.API
import Hercules.Database.Extra

elmoptions :: Options
elmoptions = Options {fieldLabelModifier = replace "'" ""}

spec :: ElmOptions -> Spec
spec elmexportoptions = Spec ["Hercules"]
            (defElmImports
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Project)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Project)
              : toElmEncoderSourceWith elmoptions      (Proxy :: Proxy Project)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Jobset)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Jobset)
              : toElmEncoderSourceWith elmoptions      (Proxy :: Proxy Jobset)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy ProjectWithJobsetsWithStatus)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy ProjectWithJobsetsWithStatus)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy JobsetWithStatus)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy JobsetWithStatus)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy JobsetStatus)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy JobsetStatus)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Jobseteval)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Jobseteval)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Job)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Job)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy JobsetevalWithStatus)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy JobsetevalWithStatus)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Jobsetevalinput)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Jobsetevalinput)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy JobsetevalWithBuilds)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy JobsetevalWithBuilds)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy JobsetWithInputs)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy JobsetWithInputs) 
              : toElmEncoderSourceWith elmoptions      (Proxy :: Proxy JobsetWithInputs)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Jobsetinputalt)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Jobsetinputalt)
              : toElmEncoderSourceWith elmoptions      (Proxy :: Proxy Jobsetinputalt)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Jobsetinput)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Jobsetinput)
              : toElmEncoderSourceWith elmoptions      (Proxy :: Proxy Jobsetinput)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Build)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Build)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy QueueSummary)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy QueueSummary)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy JobsetSummary)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy JobsetSummary)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy SystemSummary)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy SystemSummary)
              : generateElmForAPIWith elmexportoptions (Proxy :: Proxy QueryAPI)
            )

-- Generate Authorization header for Elm protected URLs
-- https://github.com/plow-technologies/servant-auth/issues/8
instance forall lang ftype api auths a.
    ( HasForeign lang ftype api
    , HasForeignType lang ftype Text
    , JWT `Elem` auths
    )
  => HasForeign lang ftype (Auth auths a :> api) where
  type Foreign ftype (Auth auths a :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) req
    where
      req = subR{ _reqHeaders = HeaderArg arg : _reqHeaders subR }
      arg = Arg
        { _argName = PathSegment "authorization"
        , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy Text)
        }

data ElmConfig = ElmConfig
  { elmpath :: String
  }

parser :: Parser ElmConfig
parser =
      ElmConfig
  <$> argument str (metavar "FOLDER")

main :: IO ()
main = do
  elmconfig <- execParser $ info (helper <*> parser)
    (fullDesc <> progDesc "Generate types for Elm frontend")
  let elmexportoptions = defElmOptions { elmExportOptions = elmoptions , urlPrefix = Dynamic }
  specsToDir [spec elmexportoptions] $ elmpath elmconfig
