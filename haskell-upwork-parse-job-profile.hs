import Data.Aeson (decode)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as L
import Upwork.Data (JobProfileResponse)

main = do
  [arg] <- getArgs
  content <- L.readFile arg
  print (decode content :: Maybe JobProfileResponse)
