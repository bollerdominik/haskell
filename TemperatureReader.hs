{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
{--
TemplateHaskell, DeriveDataTypeable and TypeFamilies is necessary for the functioning of acidstate.
--}
module Main where

import Happstack.Server               (simpleHTTP, nullConf, dir, serveDirectory, Browsing ( DisableBrowsing ))
import HtmlServer
import Parser
import Network.HTTP.Conduit           (httpLbs, parseUrl, withManager, responseBody)
import Data.List                      (sortBy)
import Data.Ord                       (comparing)
import Data.Acid                      (Query, Update, query, update, makeAcidic, openLocalState)
import Data.SafeCopy                  (base, deriveSafeCopy)
import Data.Typeable                  (Typeable)
import Control.Applicative            ((<$>))
import Control.Monad                  (msum, unless)
import Control.Monad.Reader           (ask)
import Control.Monad.State.Class      (modify)
import Control.Monad.Trans.Resource   (runResourceT)

{--
Using Network.HTTP.Conduit to grab the remote JSON data at the start of a new server session.
httpLbs is a simpler version. In this instance it is partially applied
before going to withManager which supplies it with a Manager which keeps track of open connections
and keep-alives. runResourceT unpacks everything at the end and cleans up. withManager gives httpLbs
its ResourceT Monad context. responseBody is a helper to extract the Response data type.
The output from parseUri is :: IO (Maybe Parser.Readings)
--}
parseUri :: FromJSON a => IO (Maybe a)
parseUri = do
  initReq <- parseUrl "http://www.phoric.eu/temperature.json"
  respon <- runResourceT $ withManager $ httpLbs initReq
  return $ Parser.decode (responseBody respon) -- :: IO (Maybe Parser.Readings))
{-- Defined in Parser.hs, but used here also with Data.Acid
data TemperaturePoint = TemperaturePoint { temperature :: Double
                                         , temperatureTime :: UTCTime
                       } deriving (Show, Typeable)
--}

{-- This data type derives Typeable, which makes it compatible
with the Template Haskell parts below.
--}
data TemperatureDb = TemperatureDb { allTPoints :: [TemperaturePoint] }
  deriving (Typeable)

{--
Pure functions that will be used by makeAcidic via Template Haskell to do
the boilerplate code that is needed. 
This is for the persistent data structure requirement, but without 
the supporting types and functions ( those are supplied by template haskell).
ask retrieves the Monad environment.
--}
temperaturesOverTime :: Query TemperatureDb [TemperaturePoint]
temperaturesOverTime =
  sortBy (comparing temperatureTime) . allTPoints <$> ask

{--
This is the pure function that creates the data structure.
The template haskell transforms it into a persistent form.
--}
addTemperaturePoint :: TemperaturePoint -> Update TemperatureDb ()
addTemperaturePoint temperaturePoint = modify go
 where
  go (TemperatureDb db) = TemperatureDb $
    case db of
      []            -> [temperaturePoint]
      _ -> temperaturePoint : db

	  
{--
SafeCopy makes the code future proof. SafeCopy keeps track of the details and if we want to migrate to a new structure
both structures will be held together until there is no longer any data left in the old structure and then it can be deleted.
--}
$(deriveSafeCopy 0 'base ''TemperaturePoint)
$(deriveSafeCopy 0 'base ''TemperatureDb)

-- This turns the pure functions for updating and querying the data structures into acidstate functions and types
$(makeAcidic ''TemperatureDb ['temperaturesOverTime, 'addTemperaturePoint])

-- Unpacking first from the Maybe context and then taking the list of Temperatures
-- with the record helper function 'temperatureReadings'
unpackReadings :: Maybe Readings -> [TemperaturePoint]
unpackReadings (Just theReadings) = temperatureReadings theReadings
unpackReadings Nothing = []


main :: IO ()
main = do

  -- Open the acid state
  acidState <- openLocalState (TemperatureDb [])

  -- Get the remote data
  -- :: Maybe Parser.Readings
  readings <- parseUri      
  
  {--
	Record new TemperaturePoints
    It gets the persistentPoints first in order to compare them to new
    incoming JSON data to avoid duplication.
    The actual work is done by (update acidState (AddTemperaturePoint tp))
    AddTemperaturePoint and TemperaturesOverTime are types are
    never defined. This was done by the Template Haskell with makeAcidic.
  --}
  persistentPoints <- query acidState TemperaturesOverTime
  mapM_ ( \tp ->
			Control.Monad.unless (tp `elem` persistentPoints) $
			update acidState (AddTemperaturePoint tp)
        )
        ( unpackReadings readings )

  -- Respond to Server requests with the data.
  allPoints <- query acidState TemperaturesOverTime
  simpleHTTP nullConf $ msum [ dir "static" $ serveDirectory DisableBrowsing [] "static"  -- allows access to the css in ./static
                             , temperatureServerResponse allPoints                        -- all other requests result in being served
                             ]                                                            -- the temperature data.
