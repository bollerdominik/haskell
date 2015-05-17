Haskell Assignment 
===================


This is a Haskell program that reads a remote JSON instance of temperatures and UTC times, using Network.HTTP.Conduit, and places them into haskell data structures using Data.Aeson. Data.Acid is used to persist this data across instances, and a combination of Happstack and Text.Blaze is used to serve this persisted data to a web browser at http://localhost:8000

A simple css is used and served through happstack from './static/screen.css'

----------


Required cabal installation
-------------

 - Happstack  
 - http-conduit
 - aeson
 - acid-state

How to run
-------------------

 1. Compile using `ghc -o temperatureReader temperatureReader.hs`
 2. Run executable
 3. Access from localhost with http://localhost:8000

 
Resources
-------------------
Template Haskell

 - http://research.microsoft.com/en-us/um/people/simonpj/papers/meta-haskell/meta-haskell.pdf
 - https://wiki.haskell.org/Template_Haskell


Scrap your boilerplate

 - https://wiki.haskell.org/Scrap_your_boilerplate
