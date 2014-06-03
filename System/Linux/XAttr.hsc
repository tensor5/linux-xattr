--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  © 2013-2014 Nicola Squartini
-- License     :  BSD3
--
-- Maintainer  :  Nicola Squartini <tensor5@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- @linux-xattr@ provides bindings to the Linux syscalls for reading and
-- manipulating extended attributes
-- (@<http://man7.org/linux/man-pages/man2/setxattr.2.html setxattr>@,
-- @<http://man7.org/linux/man-pages/man2/getxattr.2.html getxattr>@,
-- @<http://man7.org/linux/man-pages/man2/listxattr.2.html listxattr>@ and
-- @<http://man7.org/linux/man-pages/man2/removexattr.2.html removexattr>@).
-- Each function in this module has two variants: one with the name prefixed by
-- \"l\" and one prefixed by \"fd\".  Both of these are identical to the
-- original version except that the \"l\"-variant does not follow symbolic link
-- but acts on the link itself, and the \"fd\"-variant take a file descriptor as
-- argument rather than a @'FilePath'@.
--
--------------------------------------------------------------------------------

module System.Linux.XAttr
    ( -- * Set extended attributes

      -- | Functions in this section call the
      -- @<http://man7.org/linux/man-pages/man2/setxattr.2.html setxattr>@
      -- syscall.

      setXAttr
    , lSetXAttr
    , fdSetXAttr

      -- * Create extended attributes

      -- | Functions in this section call the
      -- @<http://man7.org/linux/man-pages/man2/setxattr.2.html setxattr>@
      -- syscall with the flag @XATTR_CREATE@.

    , createXAttr
    , lCreateXAttr
    , fdCreateXAttr

      -- * Replace extended attributes

      -- | Functions in this section call the
      -- @<http://man7.org/linux/man-pages/man2/setxattr.2.html setxattr>@
      -- syscall with the flag @XATTR_REPLACE@.

    , replaceXAttr
    , lReplaceXAttr
    , fdReplaceXAttr

      -- * Retrive extended attributes

      -- | Functions in this section call the
      -- @<http://man7.org/linux/man-pages/man2/getxattr.2.html getxattr>@
      -- syscall.

    , getXAttr
    , lGetXAttr
    , fdGetXAttr

      -- * List extended attributes

      -- | Functions in this section call the
      -- @<http://man7.org/linux/man-pages/man2/listxattr.2.html listxattr>@
      -- syscall.

    , listXAttr
    , lListXAttr
    , fdListXAttr

      -- * Remove extended attributes

      -- | Functions in this section call the
      -- @<http://man7.org/linux/man-pages/man2/removexattr.2.html removexattr>@
      -- syscall.

    , removeXAttr
    , lRemoveXAttr
    , fdRemoveXAttr

      -- * Types for extended attributes

    , Name
    , Value
    ) where

#include <sys/xattr.h>

import           Data.ByteString       (ByteString, packCStringLen,
                                        useAsCStringLen)
import           Foreign.C             (CInt (..), CSize (..), CString,
                                        peekCStringLen, throwErrnoIfMinus1,
                                        throwErrnoIfMinus1_, withCString)
import           Foreign.Marshal.Alloc (allocaBytes)
import           Foreign.Ptr           (Ptr, nullPtr)
import           System.Posix.Types    (CSsize (..), Fd (..))

-- | Name of extended attribute.
type Name = String

-- | Value of extended attribute.
type Value = ByteString

xAttrSet :: Name
         -> Value
         -> (a -> CString -> CString -> CSize -> CInt -> IO CInt)
         -> String
         -> CInt
         -> a
         -> IO ()
xAttrSet attr value func name mode f =
    throwErrnoIfMinus1_ name $ withCString attr $ \b ->
        useAsCStringLen value $ \(c,d) ->
            func f b c (fromIntegral d) mode

-- | Set the @'Value'@ of the extended attribute identified by @'Name'@ and
-- associated with the given @'FilePath'@ in the filesystem.
setXAttr :: FilePath -> Name -> Value -> IO ()
setXAttr path attr value =
    withCString path $ xAttrSet attr value setxattr "setxattr" 0

-- | Set the @'Value'@ of the extended attribute identified by @'Name'@ and
-- associated with the given @'FilePath'@ in the filesystem (do not follow
-- symbolic links).
lSetXAttr :: FilePath -> Name -> Value -> IO ()
lSetXAttr path attr value =
    withCString path $ xAttrSet attr value lsetxattr "lsetxattr" 0

-- | Set the @'Value'@ of the extended attribute identified by @'Name'@ and
-- associated with the given file descriptor in the filesystem.
fdSetXAttr :: Fd -> Name -> Value -> IO ()
fdSetXAttr (Fd n) attr value =
    xAttrSet attr value fsetxattr "fsetxattr" 0 n

-- | Identical to @'setXAttr'@, but if the attribute already exists fail with
-- @`System.IO.Error.isAlreadyExistsError`@.
createXAttr :: FilePath -> Name -> Value -> IO ()
createXAttr path attr value =
    withCString path $
    xAttrSet attr value setxattr "setxattr" #{const XATTR_CREATE}

-- | Identical to @'lSetXAttr'@, but if the attribute already exists fail with
-- @`System.IO.Error.isAlreadyExistsError`@.
lCreateXAttr :: FilePath -> Name -> Value -> IO ()
lCreateXAttr path attr value =
    withCString path $
    xAttrSet attr value lsetxattr "lsetxattr" #{const XATTR_CREATE}

-- | Identical to @'fdSetXAttr'@, but if the attribute already exists fail with
-- @`System.IO.Error.isAlreadyExistsError`@.
fdCreateXAttr :: Fd -> Name -> Value -> IO ()
fdCreateXAttr (Fd n) attr value =
    xAttrSet attr value fsetxattr "fsetxattr" #{const XATTR_CREATE} n

-- | Identical to @'setXAttr'@, but if the attribute does not exist fail with
-- @`System.IO.Error.isDoesNotExistError`@.
replaceXAttr :: FilePath -> Name -> Value -> IO ()
replaceXAttr path attr value =
    withCString path $
    xAttrSet attr value setxattr "setxattr" #{const XATTR_REPLACE}

-- | Identical to @'lSetXAttr'@, but if the attribute does not exist fail with
-- @`System.IO.Error.isDoesNotExistError`@.
lReplaceXAttr :: FilePath -> Name -> Value -> IO ()
lReplaceXAttr path attr value =
    withCString path $
    xAttrSet attr value lsetxattr "lsetxattr" #{const XATTR_REPLACE}

-- | Identical to @'fdSetXAttr'@, but if the attribute does not exist fail with
-- @`System.IO.Error.isDoesNotExistError`@.
fdReplaceXAttr :: Fd -> Name -> Value -> IO ()
fdReplaceXAttr (Fd n) attr value =
    xAttrSet attr value fsetxattr "fsetxattr" #{const XATTR_REPLACE} n


xAttrGet :: Name
         -> (a -> CString -> CString -> CSize -> IO CSsize)
         -> String
         -> a
         -> IO Value
xAttrGet attr func name f =
    withCString attr $ \cstr ->
        do size <- throwErrnoIfMinus1 name (func f cstr nullPtr 0)
           allocaBytes (fromIntegral size) $ \p ->
               do throwErrnoIfMinus1_ name $ func f cstr p (fromIntegral size)
                  packCStringLen (p, fromIntegral size)

-- | Get the @'Value'@ of the extended attribute identified by @'Name'@ and
-- associated with the given @'FilePath'@ in the filesystem, or fail with
-- @`System.IO.Error.isDoesNotExistError`@ if the attribute does not exist.
getXAttr :: FilePath -> Name -> IO Value
getXAttr path attr =
    withCString path $ xAttrGet attr getxattr "getxattr"

-- | Get the @'Value'@ of the extended attribute identified by @'Name'@ and
-- associated with the given @'FilePath'@ in the filesystem, or fail with
-- @`System.IO.Error.isDoesNotExistError`@ if the attribute does not exist (do
-- not follow symbolic links).
lGetXAttr :: FilePath -> Name -> IO Value
lGetXAttr path attr =
    withCString path $ xAttrGet attr lgetxattr "lgetxattr"

-- | Get the @'Value'@ of the extended attribute identified by @'Name'@ and
-- associated with the given file descriptor in the filesystem, or fail with
-- @`System.IO.Error.isDoesNotExistError`@ if the attribute does not exist.
fdGetXAttr :: Fd -> Name -> IO Value
fdGetXAttr (Fd n) attr =
    xAttrGet attr fgetxattr "fgetxattr" n


xAttrList :: (a -> CString -> CSize -> IO CSsize)
          -> String
          -> a
          -> IO [Name]
xAttrList func name f =
    do size <- throwErrnoIfMinus1 name (func f nullPtr 0)
       allocaBytes (fromIntegral size) $ \p ->
           do throwErrnoIfMinus1_ name (func f p (fromIntegral size))
              str <- peekCStringLen (p, fromIntegral size)
              return $ split str
    where split "" = []
          split xs = fst c : split (tail $ snd c)
              where c = break (== '\NUL') xs

-- | Get the list of extended attribute @'Name'@s associated with the given
-- @'FilePath'@ in the filesystem.
listXAttr :: FilePath -> IO [Name]
listXAttr path = withCString path $ xAttrList listxattr "listxattr"

-- | Get the list of extended attribute @'Name'@s associated with the given
-- @'FilePath'@ in the filesystem (do not follow symbolic links).
lListXAttr :: FilePath -> IO [Name]
lListXAttr path =
    withCString path $ xAttrList llistxattr "llistxattr"

-- | Get the list of extended attribute @'Name'@s associated with the given file
-- descriptor in the filesystem.
fdListXAttr :: Fd -> IO [Name]
fdListXAttr (Fd n) =
    xAttrList flistxattr "flistxattr" n


xAttrRemove :: Name -> (a -> CString -> IO CInt) -> String -> a -> IO ()
xAttrRemove attr func name f =
    throwErrnoIfMinus1_ name $ withCString attr (func f)

-- | Remove the extended attribute identified by @'Name'@ and associated with
-- the given @'FilePath'@ in the filesystem, or fail with
-- @`System.IO.Error.isDoesNotExistError`@ if the attribute does not exist.
removeXAttr :: FilePath -> Name -> IO ()
removeXAttr path attr =
    withCString path $ xAttrRemove attr removexattr "removexattr"

-- | Remove the extended attribute identified by @'Name'@ and associated with
-- the given @'FilePath'@ in the filesystem, or fail with
-- @`System.IO.Error.isDoesNotExistError`@ if the attribute does not exist (do
-- not follow symbolic links).
lRemoveXAttr :: FilePath -> Name -> IO ()
lRemoveXAttr path attr =
    withCString path $ xAttrRemove attr lremovexattr "lremovexattr"

-- | Remove the extended attribute identified by @'Name'@ and associated with
-- the given file descriptor in the filesystem, or fail with
-- @`System.IO.Error.isDoesNotExistError`@ if the attribute does not exist.
fdRemoveXAttr :: Fd -> Name -> IO ()
fdRemoveXAttr (Fd n) attr =
    xAttrRemove attr fremovexattr "fremovexattr" n


foreign import ccall unsafe setxattr :: CString
                                     -> CString
                                     -> Ptr a
                                     -> CSize
                                     -> CInt
                                     -> IO CInt

foreign import ccall unsafe lsetxattr :: CString
                                      -> CString
                                      -> Ptr a
                                      -> CSize
                                      -> CInt
                                      -> IO CInt

foreign import ccall unsafe fsetxattr :: CInt
                                      -> CString
                                      -> Ptr a
                                      -> CSize
                                      -> CInt
                                      -> IO CInt


foreign import ccall unsafe getxattr :: CString
                                     -> CString
                                     -> Ptr a
                                     -> CSize
                                     -> IO CSsize

foreign import ccall unsafe lgetxattr :: CString
                                      -> CString
                                      -> Ptr a
                                      -> CSize
                                      -> IO CSsize

foreign import ccall unsafe fgetxattr :: CInt
                                      -> CString
                                      -> Ptr a
                                      -> CSize
                                      -> IO CSsize


foreign import ccall unsafe listxattr :: CString
                                      -> CString
                                      -> CSize
                                      -> IO CSsize

foreign import ccall unsafe llistxattr :: CString
                                       -> CString
                                       -> CSize
                                       -> IO CSsize

foreign import ccall unsafe flistxattr :: CInt
                                       -> CString
                                       -> CSize
                                       -> IO CSsize


foreign import ccall unsafe removexattr :: CString
                                        -> CString
                                        -> IO CInt

foreign import ccall unsafe lremovexattr :: CString
                                         -> CString
                                         -> IO CInt

foreign import ccall unsafe fremovexattr :: CInt
                                         -> CString
                                         -> IO CInt
