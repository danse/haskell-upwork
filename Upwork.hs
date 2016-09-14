{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Upwork where

import Web.Authenticate.OAuth
import Network.HTTP.Client
import Network.HTTP.Client.TLS( tlsManagerSettings )
import Data.Maybe( fromJust )
import Data.String( fromString )
import Data.Aeson
import Control.Monad( mzero )

type JobId = String

newtype SearchResult = SearchResult { jobs :: [JobResult] }
instance FromJSON JobResult where
  parseJSON (Object v) = JobResult <$>
                         v .: "id"
  parseJSON _ = mzero
newtype JobResult = JobResult JobId
instance FromJSON SearchResult where 
  parseJSON (Object v) = SearchResult <$>
                         v .: "jobs"
  parseJSON _ = mzero
newtype JobProfileResponse = JobProfileResponse { profile :: JobProfile }
instance FromJSON JobProfileResponse where
  parseJSON (Object v) = JobProfileResponse <$>
                         v .: "profile"
  parseJSON _ = mzero
-- there is a superfluous wrapper around the candidates array
newtype Candidates = Candidates {
  candidatesWrapper :: [Candidate]
  } deriving (Show, Read)
-- there is a superfluous wrapper around the assignments array
newtype Assignments = Assignments {
  assignmentsWrapper :: [Assignment]
  } deriving (Show, Read)
instance FromJSON Candidates where
  parseJSON (Object v) = Candidates <$>
                         v .: "candidate"
  parseJSON _ = mzero
instance FromJSON Assignments where
  parseJSON (Object v) = Assignments <$>
                         v .: "assignment"
  -- sometimes `assignments` will have as a value an empty string
  parseJSON _ = pure (Assignments [])
newtype Candidate = Candidate { createDateTs :: String } deriving (Show, Read)
instance FromJSON Candidate where
  parseJSON (Object v) = Candidate <$>
                         v .: "create_date_ts"
  parseJSON _ = mzero
data Assignment = Assignment {
  asTotalHours :: String,
  asRate :: String
  } deriving (Show, Read)
instance FromJSON Assignment where
  parseJSON (Object v) = Assignment <$>
                         v .: "as_total_hours" <*>
                         v .: "as_rate"
  parseJSON _ = mzero
data JobProfile = JobProfile {
  opTotCand :: String,
  opTitle :: String,
  id :: String,
  opContractorTier :: String,
  candidates :: Candidates,
  assignments :: Assignments,
  intervieweesTotalActive :: String,
  opHighHourlyRateAll :: String
  } deriving (Show, Read)
instance FromJSON JobProfile where
  parseJSON (Object v) = JobProfile <$>
                         v .: "op_tot_cand" <*>
                         v .: "op_title" <*>
                         v .: "ciphertext" <*>
                         v .: "op_contractor_tier" <*>
                         v .: "candidates" <*>
                         v .:? "assignments" .!= Assignments [] <*>
                         v .: "interviewees_total_active" <*>
                         v .: "op_high_hourly_rate_all"
  parseJSON _ = mzero

base = "https://www.upwork.com"
interfaceBase = base ++ "/api"
authBase = interfaceBase ++ "/auth/v1/oauth/token"
jobsLoc = interfaceBase ++ "/profiles/v2/search/jobs.json"
categoriesLoc = interfaceBase ++ "/profiles/v2/metadata/categories.json"

getJobs :: Request
getJobs = fromJust (parseUrl jobsLoc)

getJob :: JobId -> Request
getJob id = fromJust (parseUrl (interfaceBase ++ "/profiles/v1/jobs/"++id++".json"))

getCategories = fromJust (parseUrl categoriesLoc)

-- https://developers.upwork.com/?lang=python#authentication_oauth-10
makeOAuth key secret = 
  def {
    oauthRequestUri = authBase ++ "/request",
    oauthAccessTokenUri = authBase ++ "/access",
    oauthAuthorizeUri = base ++ "/services/api/auth",
    oauthConsumerKey = key,
    oauthConsumerSecret = secret
    }

getCredential :: OAuth -> IO Credential
getCredential oauth = do
  manager <- newManager tlsManagerSettings
  temporaryCredential <- getTemporaryCredential oauth manager
  putStrLn "please go to:"
  putStrLn (authorizeUrl oauth temporaryCredential)
  putStrLn "after accepting, paste the verifier code below and hit enter"
  verifier <- getLine
  putStrLn $ "you pasted `"++ verifier ++"`"
  getAccessToken oauth (injectVerifier (fromString verifier) temporaryCredential) manager

askForJobs oauth tokenCredentials query = do
  signed <- signOAuth oauth tokenCredentials ((setQueryString query) getJobs)
  manager <- newManager tlsManagerSettings
  httpLbs signed manager

askForJob oauth tokenCredentials id = do
  signed <- signOAuth oauth tokenCredentials (getJob id)
  manager <- newManager tlsManagerSettings
  httpLbs signed manager

askForCategories oauth tokenCredentials = do
  signed <- signOAuth oauth tokenCredentials getCategories
  manager <- newManager tlsManagerSettings
  httpLbs signed manager

