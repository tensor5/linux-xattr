{-# LANGUAGE ForeignFunctionInterface #-}

{- |XAttr provides bindings to the glibc functions for reading and
manipulating extended attributes (@setxattr@, @getxattr@, @listxattr@,
...).  Each function in this module has two variants: the one with the
name prefixed by \"l\" and \"fd\".  Both of these are identical to the
original version except that the \"l\"-variant does not follow
symbolic link but acts on the link itself, and the \"fd\"-variant take
a file descriptor as argument rather than a @'FilePath'@.  -}

module System.Linux.XAttr
    ( -- * Set extended attributes
      setXAttr
    , lSetXAttr
    , fdSetXAttr
      -- * Create extended attributes
    , createXAttr
    , lCreateXAttr
    , fdCreateXAttr
      -- * Replace extended attributes
    , replaceXAttr
    , lReplaceXAttr
    , fdReplaceXAttr
      -- * Retrive extended attributes
    , getXAttr
    , lGetXAttr
    , fdGetXAttr
      -- * List extended attributes
    , listXAttr
    , lListXAttr
    , fdListXAttr
      -- * Remove extended attributes
    , removeXAttr
    , lRemoveXAttr
    , fdRemoveXAttr
    ) where

#include <sys/xattr.h>

import Data.ByteString (ByteString, packCStringLen, useAsCStringLen)
import Foreign.C ( CInt(..), CSize(..), CString, peekCStringLen
                 , throwErrnoIfMinus1, throwErrnoIfMinus1_, withCString)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Marshal (allocaBytes)
import System.Posix.Types (CSsize(..), Fd(..))

xAttrSet :: String
         -> ByteString
         -> (a -> CString -> Ptr () -> CSize -> CInt -> IO CInt)
         -> String
         -> CInt
         -> a
         -> IO ()
xAttrSet attr value func name mode f =
    throwErrnoIfMinus1_ name $ withCString attr $ \b ->
        useAsCStringLen value $ \(c,d) ->
            func f b (castPtr c) (fromIntegral d) mode

-- | Set the value of an extended attribute.
setXAttr :: FilePath    -- ^ target file
         -> String      -- ^ name of attribute to set
         -> ByteString  -- ^ value of attribute
         -> IO ()
setXAttr path attr value =
    withCString path $ xAttrSet attr value c_setxattr "setxattr" 0

-- | Set the value of an extended attribute (do not follow symbolic
-- links).
lSetXAttr :: FilePath -> String -> ByteString -> IO ()
lSetXAttr path attr value =
    withCString path $ xAttrSet attr value c_lsetxattr "lsetxattr" 0

-- | Set the value of an extended attribute.
fdSetXAttr :: Fd -> String -> ByteString -> IO ()
fdSetXAttr (Fd n) attr value =
    xAttrSet attr value c_fsetxattr "fsetxattr" 0 n

-- | Identical to @'setXAttr'@, but if the attribute already exists
-- fail and set errno to EEXIST.
createXAttr :: FilePath -> String -> ByteString -> IO ()
createXAttr path attr value =
    withCString path $
    xAttrSet attr value c_setxattr "setxattr" #{const XATTR_CREATE}

-- | Identical to @'lSetXAttr'@, but if the attribute already exists
-- fail and set errno to EEXIST.
lCreateXAttr :: FilePath -> String -> ByteString -> IO ()
lCreateXAttr path attr value =
    withCString path $
    xAttrSet attr value c_lsetxattr "lsetxattr" #{const XATTR_CREATE}

-- | Identical to @'fdSetXAttr'@, but if the attribute already exists
-- fail and set errno to EEXIST.
fdCreateXAttr :: Fd -> String -> ByteString -> IO ()
fdCreateXAttr (Fd n) attr value =
    xAttrSet attr value c_fsetxattr "fsetxattr" #{const XATTR_CREATE} n

-- | Identical to @'setXAttr'@, but if the attribute does not exist
-- fail and set errno to ENOATTR.
replaceXAttr :: FilePath -> String -> ByteString -> IO ()
replaceXAttr path attr value =
    withCString path $
    xAttrSet attr value c_setxattr "setxattr" #{const XATTR_REPLACE}

-- | Identical to @'lSetXAttr'@, but if the attribute does not exist
-- fail and set errno to ENOATTR.
lReplaceXAttr :: FilePath -> String -> ByteString -> IO ()
lReplaceXAttr path attr value =
    withCString path $
    xAttrSet attr value c_lsetxattr "lsetxattr" #{const XATTR_REPLACE}

-- | Identical to @'fdSetXAttr'@, but if the attribute does not exist
-- fail and set errno to ENOATTR.
fdReplaceXAttr :: Fd -> String -> ByteString -> IO ()
fdReplaceXAttr (Fd n) attr value =
    xAttrSet attr value c_fsetxattr "fsetxattr" #{const XATTR_REPLACE} n


xAttrGet :: String
         -> (a -> CString -> Ptr () -> CSize -> IO CSsize)
         -> String
         -> a
         -> IO ByteString
xAttrGet attr func name f =
    withCString attr $ \cstr ->
        do size <- throwErrnoIfMinus1 name (func f cstr nullPtr 0)
           allocaBytes (fromIntegral size) $ \p ->
               do throwErrnoIfMinus1_ name $ func f cstr p (fromIntegral size)
                  packCStringLen (castPtr p, fromIntegral size)

-- | Get the value of an extended attribute.
getXAttr :: FilePath     -- ^ target file
         -> String       -- ^ name of the attribute
         -> IO ByteString  -- ^ value of the attribute
getXAttr path attr =
    withCString path $ xAttrGet attr c_getxattr "getxattr"

-- | Get the value of an extended attribute (do not follow symbolic
-- links).
lGetXAttr :: FilePath -> String -> IO ByteString
lGetXAttr path attr =
    withCString path $ xAttrGet attr c_lgetxattr "lgetxattr"

-- | Get the value of an extended attribute.
fdGetXAttr :: Fd -> String -> IO ByteString
fdGetXAttr (Fd n) attr =
    xAttrGet attr c_fgetxattr "fgetxattr" n


xAttrList :: (a -> CString -> CSize -> IO CSsize)
          -> String
          -> a
          -> IO [String]
xAttrList func name f =
    do size <- throwErrnoIfMinus1 name (func f nullPtr 0)
       allocaBytes (fromIntegral size) $ \p ->
           do throwErrnoIfMinus1_ name (func f p (fromIntegral size))
              str <- peekCStringLen (p, fromIntegral size)
              return $ split str
    where split "" = []
          split xs = fst c : split (tail $ snd c)
              where c = break (== '\NUL') xs

-- | Get the list of attribute names associated with the given
-- @'FilePath'@.
listXAttr :: FilePath      -- ^ target file
          -> IO [String] -- ^ list of attribute names
listXAttr path = withCString path $ xAttrList c_listxattr "listxattr"

-- | Get the list of attribute names associated with the given
-- @'FilePath'@ (do not follow symbolic links).
lListXAttr :: FilePath -> IO [String]
lListXAttr path =
    withCString path $ xAttrList c_llistxattr "llistxattr"

-- | Get the list of attribute names associated with the given file
-- descriptor.
fdListXAttr :: Fd -> IO [String]
fdListXAttr (Fd n) =
    xAttrList c_flistxattr "flistxattr" n


xAttrRemove :: String -> (a -> CString -> IO CInt) -> String -> a -> IO ()
xAttrRemove attr func name f =
    throwErrnoIfMinus1_ name $ withCString attr (func f)

-- | Remove an extended attribute from the given @'FilePath'@.
removeXAttr :: FilePath -- ^ target file
            -> String   -- ^ name of the attribute
            -> IO ()
removeXAttr path attr =
    withCString path $ xAttrRemove attr c_removexattr "removexattr"

-- | Remove an extended attribute from the given @'FilePath'@ (do not follow
-- symbolic links).
lRemoveXAttr :: FilePath -> String -> IO ()
lRemoveXAttr path attr =
    withCString path $ xAttrRemove attr c_lremovexattr "lremovexattr"

-- | Remove an extended attribute from the given file descriptor.
fdRemoveXAttr :: Fd -> String -> IO ()
fdRemoveXAttr (Fd n) attr =
    xAttrRemove attr c_fremovexattr "fremovexattr" n


foreign import ccall unsafe "setxattr" c_setxattr :: CString
                                                  -> CString
                                                  -> Ptr ()
                                                  -> CSize
                                                  -> CInt
                                                  -> IO CInt

foreign import ccall unsafe "lsetxattr" c_lsetxattr :: CString
                                                    -> CString
                                                    -> Ptr ()
                                                    -> CSize
                                                    -> CInt
                                                    -> IO CInt

foreign import ccall unsafe "fsetxattr" c_fsetxattr :: CInt
                                                    -> CString
                                                    -> Ptr ()
                                                    -> CSize
                                                    -> CInt
                                                    -> IO CInt


foreign import ccall unsafe "getxattr" c_getxattr :: CString
                                                  -> CString
                                                  -> Ptr ()
                                                  -> CSize
                                                  -> IO CSsize

foreign import ccall unsafe "lgetxattr" c_lgetxattr :: CString
                                                    -> CString
                                                    -> Ptr ()
                                                    -> CSize
                                                    -> IO CSsize

foreign import ccall unsafe "fgetxattr" c_fgetxattr :: CInt
                                                    -> CString
                                                    -> Ptr ()
                                                    -> CSize
                                                    -> IO CSsize


foreign import ccall unsafe "listxattr" c_listxattr :: CString
                                                    -> CString
                                                    -> CSize
                                                    -> IO CSsize

foreign import ccall unsafe "llistxattr" c_llistxattr :: CString
                                                      -> CString
                                                      -> CSize
                                                      -> IO CSsize

foreign import ccall unsafe "flistxattr" c_flistxattr :: CInt
                                                      -> CString
                                                      -> CSize
                                                      -> IO CSsize


foreign import ccall unsafe "removexattr" c_removexattr :: CString
                                                        -> CString
                                                        -> IO CInt

foreign import ccall unsafe "lremovexattr" c_lremovexattr :: CString
                                                          -> CString
                                                          -> IO CInt

foreign import ccall unsafe "fremovexattr" c_fremovexattr :: CInt
                                                          -> CString
                                                          -> IO CInt
