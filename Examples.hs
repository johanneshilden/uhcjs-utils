module Main where

import Language.UHC.JS.ECMA.String
import Language.UHC.JS.Primitives
import Language.UHC.JS.Types
import Language.UHC.JS.W3C.HTML5

import Util.Core
import Util.Handlebars

import qualified Util.DOM                  as U

data Person = P { name :: !JSString }

data PersonList = PL 
    { people :: !(Collection Person) }

testTemplate :: IO ()
testTemplate = do
    f <- compile "<p>Name: {{name}}</p>"
    out <- render f $ P { name = (toJS "Alen") }
    bodyAppendHtml out

testCollections :: IO ()
testCollections = do
    f <- compile "<h1>List of people</h1><ul>{{#each people}}<li>Name: {{name}}</li>{{/each}}</ul>"

    collection <- listToCollection 
        [ P (toJS "Bob")
        , P (toJS "Rod")
        ]
    out <- render f $ PL collection

    bodyAppendHtml out

main :: IO ()
main = do
    wrap $ do
        -- Run tests
        testTemplate
        testCollections
    >>= onLoad

-- Some helper functions for the examples

getBody :: IO Node
getBody = do
    doc  <- document
    list <- documentGetElementsByTagName doc "body"
    nodeListItem list 0

bodyAppendHtml :: String -> IO ()
bodyAppendHtml out = do
    body <- getBody
    e  <- documentCreateElement "div"
    e' <- U.setInnerHtml e out
    elementAppendChild body e'

