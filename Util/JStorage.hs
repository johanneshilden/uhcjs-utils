module Util.JStorage 
    ( Storable
    , set
    , get
    , getTypeInfo
    , deleteKey
    , index
    , flush
    , setTTL
    , getTTL
    , storageSize
    , currentBackend
    , reInit
    , storageAvailable
    ) where

import Language.UHC.JS.ECMA.String
import Language.UHC.JS.Primitives
import Language.UHC.JS.Types
import Util.Core
 
data Box a = Box a

-- | Saves a value to local storage.
set :: Storable a 
    => String   -- ^ The key
    -> a        -- ^ The value to store
    -> IO ()
set ('_':'_':_) _ = error "Invalid key: Name reserved for internal use."
set key val = do
    obj <- mkObj $ Box val
    _set (toJS key) obj
    setTypeInfo key val

setTypeInfo :: Storable a => String -> a -> IO ()
setTypeInfo key val = do
    let t = tag val
    __set (toJS $ "__" ++ key) (stringToJSString t)

-- | Retrieves the stored value matching the given key,
-- if one exists.
get :: Storable a 
    => String        -- ^ The key
    -> IO (Maybe a)
get key = do
    ttg <- getTypeInfo key
    sg  <- safeGet $ toJS key
    return $ case sg of
        (Just (Box val)) | tag val == ttg -> Just val
        _                                -> Nothing

-- | For every object, an associated type tag is stored with the key name 
-- prefixed by two underscore characters (e.g. __someKey).
getTypeInfo :: String -> IO String
getTypeInfo key = do 
    x <- __get (toJS $ "__" ++ key) 
              (stringToJSString "-") 
    return $ jsStringToString x

-- | Removes a key from the storage.
deleteKey :: String -> IO ()
deleteKey key = _delete (toJS key) >> _delete (toJS $ "__" ++ key)

-- | Returns all keys currently in use as a list.
index :: IO [String]
index = do
    i <- _index
    return $ case _indexLength i of
        0 -> []
        n -> [fromJS $ _indexElement i x | x <- [0 .. n - 1]]

-- | Clears the cache.
flush :: IO ()
flush = _flush

-- | Sets a TTL (in milliseconds) for an existing key. Use 0 or 
-- negative value to clear TTL.
setTTL :: String -> Int -> IO ()
setTTL key ttl = _setTTL (toJS key) ttl

-- | Gets remaining TTL (in milliseconds) for a key or 0 if not TTL 
-- has been set.
getTTL :: String -> IO Int
getTTL key = _getTTL (toJS key)

-- | Returns the size of the stored data in bytes.
storageSize :: IO Int
storageSize = _storageSize

-- | Returns the storage engine currently in use or 'false' if none.
currentBackend :: IO String
currentBackend = _currentBackend >>= return . fromJS 

-- | Reloads the data from browser storage.
reInit :: IO ()
reInit = _reInit

-- | Returns True if storage is available.
storageAvailable :: IO Bool
storageAvailable = do
    b <- _storageAvailable
    return $ b /= 0

safeGet :: JSString -> IO (Maybe a)
safeGet key = do
    x <- _get key
    return $ case _notNull x of
        0 -> Nothing
        _ -> Just x

foreign import js "$.jStorage.get(%1)"
    _get :: JSString -> IO a

foreign import js "$.jStorage.get(%1, %2)"
    __get :: JSString -> a -> IO a

foreign import js "$.jStorage.set(%1, %2)"
    _set :: JSString -> JSPtr a -> IO ()

foreign import js "$.jStorage.set(%1, %2)"
    __set :: JSString -> a -> IO ()

foreign import js "$.jStorage.deleteKey(%1)"
    _delete :: JSString -> IO ()

data IndexPtr
data Index = JSPtr IndexPtr

foreign import js "$.jStorage.index()"
    _index :: IO Index

foreign import js "%1[%2]"
    _indexElement :: Index -> Int -> JSString

foreign import js "%1.length"
    _indexLength :: Index -> Int

foreign import js "$.jStorage.flush()"
    _flush :: IO ()

foreign import js "Boolean(%1)"
    _notNull :: a -> Int

foreign import js "$.jStorage.setTTL(%*)"
    _setTTL :: JSString -> Int -> IO ()

foreign import js "$.jStorage.getTTL(%1)"
    _getTTL :: JSString -> IO Int

foreign import js "$.jStorage.storageSize()"
    _storageSize :: IO Int

foreign import js "$.jStorage.currentBackend()"
    _currentBackend :: IO JSString

foreign import js "$.jStorage.reInit()"
    _reInit :: IO ()

foreign import js "$.jStorage.storageAvailable()"
    _storageAvailable :: IO Int

class Storable a where
    tag :: a -> String 

instance Storable Int where
    tag _ = "Int"

instance Storable JSString where
    tag _ = "String"

instance Storable Float where
    tag _ = "Float"

