module Util.Core where

import Language.UHC.JS.Primitives

foreign import js "wrapper"
    wrap :: IO () -> IO (JSFunPtr (IO ()))

foreign import js "window.addEventListener('load', %1)"
    onLoad :: JSFunPtr (IO ()) -> IO ()

