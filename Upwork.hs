{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Upwork where

import Web.Authenticate.OAuth
import Network.HTTP.Client
import Network.HTTP.Client.TLS( tlsManagerSettings )
import qualified Data.ByteString.Lazy.Char8 as C
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
newtype JobProfileResponse = JobProfileResponse {
  profile :: JobProfile
  }
instance FromJSON JobProfileResponse where
  parseJSON (Object v) = JobProfileResponse <$>
                         v .: "profile"
  parseJSON _ = mzero
data JobProfile = JobProfile {
  opTotCand :: String,
  opTitle :: String,
  cyphertext :: String
  } deriving Show
instance FromJSON JobProfile where
  parseJSON (Object v) = JobProfile <$>
                         v .: "op_tot_cand" <*>
                         v .: "op_title" <*>
                         v .: "ciphertext"
  parseJSON _ = mzero

base = "https://www.upwork.com"
interfaceBase = base ++ "/api"
authBase = interfaceBase ++ "/auth/v1/oauth/token"
jobsLoc = interfaceBase ++ "/profiles/v2/search/jobs.json"

getJobs :: Request
getJobs = fromJust (parseUrl jobsLoc)

getJob :: JobId -> Request
getJob id = fromJust (parseUrl (interfaceBase ++ "/profiles/v1/jobs/"++id++".json"))


queryAll = setQueryString [("q", Just "*")]

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

askForJobs oauth tokenCredentials = do
  signed <- signOAuth oauth tokenCredentials (queryAll getJobs)
  manager <- newManager tlsManagerSettings
  response <- httpLbs signed manager
  C.putStrLn (responseBody response)

askForJob oauth tokenCredentials id = do
  signed <- signOAuth oauth tokenCredentials (getJob id)
  manager <- newManager tlsManagerSettings
  response <- httpLbs signed manager
  C.putStrLn (responseBody response)
