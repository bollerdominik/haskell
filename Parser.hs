{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveDataTypeable #-}
{--
RecordWildCards -- used to allow double period in module declaration of records.
OverLoadedStrings -- described in HtmlServer.hs
--}
module Parser ( decode                      -- Comes from the Data.Aeson module and re-exported here
              , TemperaturePoint(..)        -- (..) is used to includes all record labels,
              , Readings(..)                -- is only available with the GHC language extension
              , FromJSON(..)                -- RecordWildCards.
              ) where

import Data.Aeson             --(Object, FromJSON, decode, parseJSON, (.:))
import Data.Typeable          (Typeable)
import Data.Time              (parseTime, UTCTime)
import System.Locale          (defaultTimeLocale)
import Control.Applicative    ((<$>), (<*>))
import Data.Monoid            (mempty)

data TemperaturePoint = TemperaturePoint { temperatureTime :: UTCTime
                                         , temperature :: Double
                                         }
                        deriving (Show, Eq, Typeable)


data Readings = Readings {
  temperatureReadings :: [TemperaturePoint]
} deriving (Eq, Show)

{--
There is no instance of parseJSON to
handle the particular formatting of the UTCTime input. 
parseTime parses a string to UTCTime, defaultTimeLocale returns a TimeLocale , the string
is the expected time format, and t the input string.
--}
simpleParseTime :: String -> UTCTime
simpleParseTime t =
  case parseTime defaultTimeLocale "%FT%X%z" t of
    Just d -> d
    Nothing ->
      case parseTime defaultTimeLocale "%FT%T%QZ" t of
        Just d -> d
        Nothing -> error "could not parse date"

{--
When a request is made to decode and this request matches the structure of this instance declaration,
then it will be used. This handles the outermost layer of the JSON data structure. The
call to 'parseJSON =<< (o .: "temperatures")' bound to temperatureReadings, calls the
next instance declaration below. The output of parseJSON is 'Parser a', where Parser is a Monad. The
binding in the 'do' strips the Parser away. So it is just a list of TemperaturePoint, that is then
applied to the data constructor Readings.
(.:) :: FromJSON a => Object -> Text -> Parser a
This takes an Object and some Text to match (non-inline sytax), and returns a Parser
filled with 'a' whatever the context sets the type of 'a' to be.
The output of (o .: "temperatures") is :: Parser [something], then the =<< operator pulls
[something] out of it's Parser Monad context and gives it to parseJSON, which then uses
the instance below to pull data out of the rest of the JSON structure, and returns
:: Parser [TemperaturePoint] which the bind <- takes out of Parser and puts in temperatureReadings.
The constructor Readings then takes it and creates Readings [TemperaturePoint], the return wraps
everything in Parser, so that the final output is Parser Readings [TemperaturePoint]
--}
instance FromJSON Readings where
  parseJSON (Object o) = do
    temperatureReadings <- parseJSON =<< (o .: "temperatures")
    return $ Readings temperatureReadings       -- :: Parser Readings [TemperaturePoint]
  parseJSON _ = mempty

{--
The function handles the conversion from string to UTCTime manually, then everything is passed
to the data constructor TemperaturePoint, and then everything has a type. 
parseJSON _ = mempty catches anything that doesn't match what is being looked for.
--}
instance FromJSON TemperaturePoint where
  parseJSON (Object v) = TemperaturePoint <$>
                        (simpleParseTime <$> v .: "date") <*>
                        v .: "temperature"
  parseJSON _ = mempty

