module Util.DOM 
    ( getElementById
    , nodeIsNull
    , parentNode
    , createTextNode
    , setInnerHtml
    ) where

import Language.UHC.JS.Prelude
import Language.UHC.JS.Primitives
import Language.UHC.JS.Types
import Language.UHC.JS.W3C.HTML5

getElementById :: String -> IO (Maybe Node)
getElementById name = 
    (_getElementById $ toJS name) >>= maybeNode

foreign import js "document.getElementById(%1)"
    _getElementById :: JSString -> IO Node

nodeIsNull :: Node -> IO Bool
nodeIsNull n = _notNull n >>= return . (==) 0 

foreign import js "Boolean(%1)"
    _notNull :: Node -> IO Int

parentNode :: Node -> IO (Maybe Node)
parentNode el = (_parentElement el) >>= maybeNode

foreign import js "%1.parentElement"
    _parentElement :: Node -> IO Node

maybeNode :: Node -> IO (Maybe Node)
maybeNode el = do
    nn <- nodeIsNull el
    return $ if nn
        then Nothing
        else Just el

createTextNode :: String -> IO Node
createTextNode = _createTextNode . toJS

foreign import js "document.createTextNode(%1)"
    _createTextNode :: JSString -> IO Node

setInnerHtml :: Node -> String -> IO Node
setInnerHtml e html = setAttr "innerHTML" (toJS html :: JSString) e

