module Main where

import Language.UHC.JS.Assorted
import Util.Core
import Util.Router

showPage :: String -> IO ()
showPage x = alert $ "Page : " ++ x

staticP :: IO ()
staticP = alert "a static page"

staticP2 :: IO ()
staticP2 = alert "another page"

showComment :: String -> String -> IO ()
showComment p c = alert $ 
    "post : " ++ (show p) ++ ", comment : " ++ (show c)

defRoute :: IO ()
defRoute = alert "default"

main :: IO ()
main = do
    wrap $ setMap
        [ -- /page/:id
          "page" ~/~ showPage
          -- /static-page
        , "static-page" /~/ staticP
          -- /show/me/the/page
        , "show" /// "me" /// "the" /// "page" /~/ staticP2
          -- /post/:pid/comment/:cid
        , "post" </< \p -> "comment" ~/~ \c -> showComment p c
          -- default route
        ] defRoute
    >>= onLoad

