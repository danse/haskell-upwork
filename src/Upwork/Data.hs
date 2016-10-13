{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Upwork.Data where

import Web.Authenticate.OAuth
import Network.HTTP.Client
import Network.HTTP.Client.TLS( tlsManagerSettings )
import Data.Maybe( fromJust )
import Data.String( fromString )
import Data.Aeson
import Data.Aeson.Types( Parser )
import Control.Monad( mzero )
import Data.Foldable( toList )

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
-- assignments can be one or many
data OneOrMany a = One a | Many [a] deriving (Show, Read)
parseJSONOneOrMany :: FromJSON a => Value -> Parser (OneOrMany a)
parseJSONOneOrMany (Object v) = One <$> parseJSON (Object v)
parseJSONOneOrMany (Array v)  = Many <$> mapM parseJSON (toList v)
parseJSONOneOrMany _ = mzero
instance (FromJSON a) => FromJSON (OneOrMany a) where parseJSON = parseJSONOneOrMany
-- there is a superfluous wrapper around the assignments array
newtype Assignments = Assignments {
  assignmentsWrapper :: OneOrMany Assignment
  } deriving (Show, Read)
instance FromJSON Candidates where
  parseJSON (Object v) = Candidates <$>
                         v .: "candidate"
  parseJSON _ = mzero
instance FromJSON Assignments where
  parseJSON (Object v) = Assignments <$>
                         v .: "assignment"
  -- sometimes `assignments` will have as a value an empty string
  parseJSON _ = pure (Assignments (Many []))
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
                         v .:? "assignments" .!= Assignments (Many []) <*>
                         v .: "interviewees_total_active" <*>
                         v .: "op_high_hourly_rate_all"
  parseJSON _ = mzero
