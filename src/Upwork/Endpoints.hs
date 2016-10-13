{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Upwork.Endpoints where

import Web.Authenticate.OAuth
import Network.HTTP.Client
import Network.HTTP.Client.TLS( tlsManagerSettings )
import Data.Maybe( fromJust )
import Data.String( fromString )
import Data.Aeson
import Data.Aeson.Types( Parser )
import Control.Monad( mzero )
import Data.Foldable( toList )
import Upwork.Data( JobId )

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

