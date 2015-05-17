{-# LANGUAGE OverloadedStrings #-}    -- This language extension means string literals "example"
                                      -- are a call to toString of the IsString type class
module HtmlServer where

import Happstack.Server                                   (ServerPart, Response, toResponse, ok)
import           Text.Blaze                               ((!))
import qualified Text.Blaze.Html4.Strict as H             (html, head, meta, body, title, link, table, td, tr
                                                          , p, b, big, toHtml, string, toMarkup, ToMarkup, Html)
import qualified Text.Blaze.Html4.Strict.Attributes as A  (httpEquiv, content, name, rel, type_, href)
import Parser                                             (TemperaturePoint, temperature, temperatureTime)
import Data.Time                                          (formatTime, UTCTime)
import System.Locale                                      (defaultTimeLocale)
import Data.Monoid                                        (mappend)

-- This instance allows H.toHtml to handle UTCTime values
instance H.ToMarkup UTCTime where
  toMarkup = H.string . formatTime defaultTimeLocale "%X on %F at UTC offset %z"

{--
Takes a list of TemperaturePoint's and turns them into a html table. The return type Html is a type synonym of
Markup. Markupm() is definded as monad which allows the use of 'do' notation below, which is more compact.
Each line of the 'do' notation is separated by (>>) which is equivalent to mappend.
(H.td . H.big . H.toHtml . H.string) "exampleString" becomes <td><big>exampleString<big><td> :: H.Html
The functions temperatureTime and temperature are record syntax functions for getting those records from the
TemperaturePoint type. They are zipped together to form the second input to the map function, a list of tuples
[(UTCTime, Double)], the lambda function takes each part of the tuple and puts it into a <td></td> pair and appends them
together. now there is a list of these Html td tags. mapM_ is used to encapsulate each of these inside H.tr tags and change
the structure from a list to one big Html. The do notation puts the two lines together.
--}
myTable :: [TemperaturePoint] -> H.Html
myTable xs = H.table $ do
  H.tr (mappend ((H.td . H.big . H.toHtml . H.string) "Date and time") ((H.td . H.big . H.toHtml . H.string) "Temperature (°C)"))
  mapM_ H.tr $ map (\(ttime, t) -> (mappend ((H.td . H.toHtml) ttime) ((H.td . H.toHtml) t)))
                     (zip (map temperatureTime xs) (map temperature xs))

{--
This function takes a String representing the title, a list of H.Html representing the headers, and a H.Html for the body.
The 'do' notation puts separate H.Html variables together with Append (is done by mapped). 
The function appTemplate is a template. The H.Html returned is constructed from the three inputs,
while the Attributes A.httpEquiv and A.content are hard coded into the template.
--}
appTemplate :: String -> [H.Html] -> H.Html -> H.Html
appTemplate title headers bdy =
    H.html $ do
      H.head $ do
        H.title (H.toHtml title)
        H.meta ! A.httpEquiv "Content-Type"
               ! A.content "text/html;charset=utf-8"
        sequence_ headers
      H.body bdy

{--
This is where the template is applied. For (ok :: FilterMonad Response m => a -> m a), the type constraint
'FilterMonad Response' means that the Monad 'm' in the signature must be of that type, and is implemented by
ServerPartT which has a specific instance which matches the return type of temperatureServerResponse.
'ServerPart Response' is a type synonym for ServerPartT IO Response. This function supplies the context.
The function (toResponse :: a -> Response) has an instance for Html, so it's signature is really
(toResponse :: H.Html -> Response) in this context. The three arguments of 'appTemplate'
are there, a string "", a list [H.Html], and a H.Html
--}
temperatureServerResponse :: [TemperaturePoint] -> ServerPart Response
temperatureServerResponse temperaturePoints =
   ok $ toResponse $
    appTemplate "Temperature Readings"
                [H.meta ! A.name "keywords"
                         ! A.content "temperatures",
                 H.link ! A.rel "stylesheet"
                         ! A.type_ "text/css"
                         ! A.href "/static/screen.css"
                ]
                (H.b $ do _ <- "Temperature Readings ordered by date" 	-- Note: _ <- is in order to
                          H.p (H.b (myTable temperaturePoints)))      	-- suppresses a warning.

