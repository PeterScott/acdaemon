{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Trie (insertPair, deleteKey, autocomplete) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy.Char8 as L
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import System.Exit (exitFailure)

-- Import the functions for dealing with the huge global trie data
-- structure from C, where they're defined.
foreign import ccall unsafe "insert"
     c_insert :: CString -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "delete"
     c_delete :: CString -> CInt -> IO CInt

foreign import ccall unsafe "ac_start"
     c_ac_start :: CString -> CInt -> IO (Ptr CInt)

foreign import ccall unsafe "ac_next"
     c_ac_next :: IO (Ptr CInt)

foreign import ccall unsafe "get_index"
     c_get_index :: CString -> CInt -> IO CString

panic :: IO ()
panic = do
  putStrLn "unknown fatal error"
  exitFailure

-- | Insert a (string, value) pair.
insertPair :: ByteString -> Int -> IO ()
insertPair bs value = B.unsafeUseAsCString bs go
    where go cstr = do
            status <- c_insert cstr (fromIntegral $ B.length bs) (fromIntegral value)
            if status /= 0 then panic else return ()

-- | Delete a string. Returns True if the string was successfully
-- deleted, or False if it did not exist in the first place.
deleteKey :: ByteString -> IO Bool
deleteKey bs = B.unsafeUseAsCString bs go
    where go cstr = do
            status <- c_delete cstr (fromIntegral $ B.length bs)
            if status /= 0 then return False else return True

-- | Start an autocomplete search. Returns string and value if any was
-- found.
ac_start :: ByteString -> IO (Maybe (ByteString, Int))
ac_start bs = B.unsafeUseAsCString bs go
    where go cstr = do
            valuePtr <- c_ac_start cstr (fromIntegral $ B.length bs)
            if valuePtr == nullPtr
              then return Nothing
              else do indexPtr <- c_get_index cstr (fromIntegral $ B.length bs)
                      if indexPtr == nullPtr
                        then return Nothing
                        else do value <- peek valuePtr
                                result <- B.packCString indexPtr
                                return (Just (result, fromIntegral value))

-- | Continue an autocomplete search. Returns string and value if any
-- was found.
ac_next :: ByteString -> IO (Maybe (ByteString, Int))
ac_next bs = B.unsafeUseAsCString bs go
    where go cstr = do
            valuePtr <- c_ac_next
            if valuePtr == nullPtr
              then return Nothing
              else do indexPtr <- c_get_index cstr (fromIntegral $ B.length bs)
                      if indexPtr == nullPtr
                        then return Nothing
                        else do value <- peek valuePtr
                                result <- B.packCString indexPtr
                                return (Just (result, fromIntegral value))

-- | Perform an autocomplete search, returning a lazy list of (string,
-- value) pairs.
autocomplete :: ByteString -> IO [(ByteString, Int)]
autocomplete bs = do ac_start bs >>= goVal                     
    where go = ac_next bs >>= goVal
          goVal val = case val of
                        Nothing -> return []
                        Just x  -> do tl <- go
                                      return $ x : tl
