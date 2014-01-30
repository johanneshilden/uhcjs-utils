module Util.Router 
    ( Route 
    , locationHash
    , trimHash
    , components
    , setMap
    , (///)
    , (/~/)
    , (</<)
    , (~/~)
    ) where

import Language.UHC.JS.Assorted
import Language.UHC.JS.ECMA.String
import Language.UHC.JS.Primitives
import Language.UHC.JS.Types
import Language.UHC.JS.W3C.HTML5
import Util.Core

-- | Return a list of the slash-separated segments of the 
-- anchor portion of the current URL.
locationHash :: IO [String]
locationHash = do
    h <- _hash
    case components $ fromJS h of
        [] -> return []
        (x:xs) -> do
            return $ if "#" == x
                then xs
                else (trimHash x:xs)

-- | Trim out the first character from a string if it is
-- a hash symbol.
trimHash :: String -> String
trimHash "" = ""
trimHash str@(x:xs) = 
    if '#' == x
        then xs
        else str

-- | Split a uri string into its path components.
components :: String -> [String]
components "" = []
components xs = f xs [] [] 
    where f [] ys zs = reverse $ map reverse (ys:zs)
          f (x:xs) ys zs | x == '/'   = f xs [] (ys:zs)
                         | otherwise = f xs (x:ys) zs

foreign import js "window.location.hash"
    _hash :: IO JSString

onHashChange :: IO ()   -- ^ A callback function
             -> IO ()
onHashChange fn = do
    cback <- wrap fn 
    win <- _window
    bindProp win cback
    return ()
  where bindProp :: Node -> JSFunPtr (IO ()) -> IO Node
        bindProp e f = setAttr "onhashchange" f e

foreign import js "window"
    _window :: IO Node

data Route = Part !String !Route
           | Arg  (String -> Route)
           | Run  (IO ())

look :: [String] -> Route -> IO Bool
look [] (Run go) = go >> return True
look (x:xs) (Arg f) = look xs $ f x
look (x:xs) (Part str r) | x == str = look xs r
look _ _ = return False

route :: IO () -> [Route] -> IO ()
route def routes = do
    locationHash >>= f routes 
  where f [] _ = def
        f (x:xs) h = do b <- look h x
                        case b of
                            True  -> return ()
                            False -> f xs h

-- | Set routing rules.
setMap :: [Route]  -- ^ Routes
       -> IO ()    -- ^ Default route
       -> IO ()
setMap r = onHashChange . flip route r 

-- Infix operators

(///) :: String -> Route -> Route 
a /// b = Part a b

(/~/) :: String -> IO () -> Route
a /~/ b = Part a $ Run b

(</<) :: String -> (String -> Route) -> Route
a </< b = Part a $ Arg b

(~/~) :: String -> (String -> IO ()) -> Route
a ~/~ b = a </< Run . b

infixr ///
infixr /~/
infixr </<
infixr ~/~

