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
-- Moreover, every function has an @/xxx/__UserXAttr__@ variant for working
-- transparently in the @__user__@ namespace of extended attributes, without
-- worrying about the @"user."@ prefix: these functions automatically prepends
-- the string @"user."@ to the @'Name'@ of the attribute when @'Name'@ is an
-- input value, or strip the prefix @"user."@ from it when @'Name'@ is a
-- returned value. See the documentation of each individual function for
-- details.
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

      -- ** Set extended @user@ attributes

    , setUserXAttr
    , lSetUserXAttr
    , fdSetUserXAttr

      -- * Create extended attributes

      -- | Functions in this section call the
      -- @<http://man7.org/linux/man-pages/man2/setxattr.2.html setxattr>@
      -- syscall with the flag @XATTR_CREATE@.

    , createXAttr
    , lCreateXAttr
    , fdCreateXAttr

      -- ** Create extended @user@ attributes

    , createUserXAttr
    , lCreateUserXAttr
    , fdCreateUserXAttr

      -- * Replace extended attributes

      -- | Functions in this section call the
      -- @<http://man7.org/linux/man-pages/man2/setxattr.2.html setxattr>@
      -- syscall with the flag @XATTR_REPLACE@.

    , replaceXAttr
    , lReplaceXAttr
    , fdReplaceXAttr

      -- ** Replace extended @user@ attributes

    , replaceUserXAttr
    , lReplaceUserXAttr
    , fdReplaceUserXAttr

      -- * Retrive extended attributes

      -- | Functions in this section call the
      -- @<http://man7.org/linux/man-pages/man2/getxattr.2.html getxattr>@
      -- syscall.

    , getXAttr
    , lGetXAttr
    , fdGetXAttr

      -- ** Retrieve extended @user@ attributes

    , getUserXAttr
    , lGetUserXAttr
    , fdGetUserXAttr

      -- * List extended attributes

      -- | Functions in this section call the
      -- @<http://man7.org/linux/man-pages/man2/listxattr.2.html listxattr>@
      -- syscall.

    , listXAttr
    , lListXAttr
    , fdListXAttr

      -- ** List extended @user@ attributes

      -- | These functions only list those extended attributes with @'Name'@
      -- beginning with @"user."@. The @"user."@ prefix is removed from each
      -- @'Name'@ in the output list.

    , listUserXAttr
    , lListUserXAttr
    , fdListUserXAttr

      -- * Remove extended attributes

      -- | Functions in this section call the
      -- @<http://man7.org/linux/man-pages/man2/removexattr.2.html removexattr>@
      -- syscall.

    , removeXAttr
    , lRemoveXAttr
    , fdRemoveXAttr

      -- ** Remove extended @user@ attributes

    , removeUserXAttr
    , lRemoveUserXAttr
    , fdRemoveUserXAttr

      -- * Types for extended attributes

    , Name
    , Value
    ) where

#include <sys/xattr.h>

import           Control.Monad         (liftM)
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

-- | @'setUserXAttr' "\/some\/path" "foo" "bar" = 'setXAttr' "\/some\/path" "user.foo" "bar"@
setUserXAttr :: FilePath -> Name -> Value -> IO ()
setUserXAttr = userXAttr setXAttr

-- | Set the @'Value'@ of the extended attribute identified by @'Name'@ and
-- associated with the given @'FilePath'@ in the filesystem (do not follow
-- symbolic links).
lSetXAttr :: FilePath -> Name -> Value -> IO ()
lSetXAttr path attr value =
    withCString path $ xAttrSet attr value lsetxattr "lsetxattr" 0

-- | @'lSetUserXAttr' "\/some\/link" "foo" "bar" = 'lSetXAttr' "\/some\/link" "user.foo" "bar"@
lSetUserXAttr :: FilePath -> Name -> Value -> IO ()
lSetUserXAttr = userXAttr lSetXAttr

-- | Set the @'Value'@ of the extended attribute identified by @'Name'@ and
-- associated with the given file descriptor in the filesystem.
fdSetXAttr :: Fd -> Name -> Value -> IO ()
fdSetXAttr (Fd n) attr value =
    xAttrSet attr value fsetxattr "fsetxattr" 0 n

-- | @'fdSetUserXAttr' ('Fd' n) "foo" "bar" = 'fdSetXAttr' ('Fd' n) "user.foo" "bar"@
fdSetUserXAttr :: Fd -> Name -> Value -> IO ()
fdSetUserXAttr = userXAttr fdSetXAttr

-- | Identical to @'setXAttr'@, but if the attribute already exists fail with
-- @`System.IO.Error.isAlreadyExistsError`@.
createXAttr :: FilePath -> Name -> Value -> IO ()
createXAttr path attr value =
    withCString path $
    xAttrSet attr value setxattr "setxattr" #{const XATTR_CREATE}

-- | @'createUserXAttr' "\/some\/path" "foo" "bar" = 'createXAttr' "\/some\/path" "user.foo" "bar"@
createUserXAttr :: FilePath -> Name -> Value -> IO ()
createUserXAttr = userXAttr createXAttr

-- | Identical to @'lSetXAttr'@, but if the attribute already exists fail with
-- @`System.IO.Error.isAlreadyExistsError`@.
lCreateXAttr :: FilePath -> Name -> Value -> IO ()
lCreateXAttr path attr value =
    withCString path $
    xAttrSet attr value lsetxattr "lsetxattr" #{const XATTR_CREATE}

-- | @'lCreateUserXAttr' "\/some\/link" "foo" "bar" = 'lCreateXAttr' "\/some\/link" "user.foo" "bar"@
lCreateUserXAttr :: FilePath -> Name -> Value -> IO ()
lCreateUserXAttr = userXAttr lCreateXAttr

-- | Identical to @'fdSetXAttr'@, but if the attribute already exists fail with
-- @`System.IO.Error.isAlreadyExistsError`@.
fdCreateXAttr :: Fd -> Name -> Value -> IO ()
fdCreateXAttr (Fd n) attr value =
    xAttrSet attr value fsetxattr "fsetxattr" #{const XATTR_CREATE} n

-- | @'fdCreateUserXAttr' ('Fd' n) "foo" "bar" = 'fdCreateXAttr' ('Fd' n) "user.foo" "bar"@
fdCreateUserXAttr :: Fd -> Name -> Value -> IO ()
fdCreateUserXAttr = userXAttr fdCreateXAttr

-- | Identical to @'setXAttr'@, but if the attribute does not exist fail with
-- @`System.IO.Error.isDoesNotExistError`@.
replaceXAttr :: FilePath -> Name -> Value -> IO ()
replaceXAttr path attr value =
    withCString path $
    xAttrSet attr value setxattr "setxattr" #{const XATTR_REPLACE}

-- | @'replaceUserXAttr' "\/some\/path" "foo" "bar" = 'replaceXAttr' "\/some\/path" "user.foo" "bar"@
replaceUserXAttr :: FilePath -> Name -> Value -> IO ()
replaceUserXAttr = userXAttr replaceXAttr

-- | Identical to @'lSetXAttr'@, but if the attribute does not exist fail with
-- @`System.IO.Error.isDoesNotExistError`@.
lReplaceXAttr :: FilePath -> Name -> Value -> IO ()
lReplaceXAttr path attr value =
    withCString path $
    xAttrSet attr value lsetxattr "lsetxattr" #{const XATTR_REPLACE}

-- | @'lReplaceUserXAttr' "\/some\/link" "foo" "bar" = 'lReplaceXAttr' "\/some\/link" "user.foo" "bar"@
lReplaceUserXAttr :: FilePath -> Name -> Value -> IO ()
lReplaceUserXAttr = userXAttr lReplaceXAttr

-- | Identical to @'fdSetXAttr'@, but if the attribute does not exist fail with
-- @`System.IO.Error.isDoesNotExistError`@.
fdReplaceXAttr :: Fd -> Name -> Value -> IO ()
fdReplaceXAttr (Fd n) attr value =
    xAttrSet attr value fsetxattr "fsetxattr" #{const XATTR_REPLACE} n

-- | @'fdReplaceUserXAttr' ('Fd' n) "foo" "bar" = 'fdReplaceXAttr' ('Fd' n) "user.foo" "bar"@
fdReplaceUserXAttr :: Fd -> Name -> Value -> IO ()
fdReplaceUserXAttr = userXAttr fdReplaceXAttr


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

-- | @'getUserXAttr' "\/some\/path" "foo" = 'getXAttr' "\/some\/path" "user.foo"@
getUserXAttr :: FilePath -> Name -> IO Value
getUserXAttr = userXAttr getXAttr

-- | Get the @'Value'@ of the extended attribute identified by @'Name'@ and
-- associated with the given @'FilePath'@ in the filesystem, or fail with
-- @`System.IO.Error.isDoesNotExistError`@ if the attribute does not exist (do
-- not follow symbolic links).
lGetXAttr :: FilePath -> Name -> IO Value
lGetXAttr path attr =
    withCString path $ xAttrGet attr lgetxattr "lgetxattr"

-- | @'lGetUserXAttr' "\/some\/link" "foo" = 'lGetXAttr' "\/some\/link" "user.foo"@
lGetUserXAttr :: FilePath -> Name -> IO Value
lGetUserXAttr = userXAttr lGetXAttr

-- | Get the @'Value'@ of the extended attribute identified by @'Name'@ and
-- associated with the given file descriptor in the filesystem, or fail with
-- @`System.IO.Error.isDoesNotExistError`@ if the attribute does not exist.
fdGetXAttr :: Fd -> Name -> IO Value
fdGetXAttr (Fd n) attr =
    xAttrGet attr fgetxattr "fgetxattr" n

-- | @'fdGetUserXAttr' ('Fd' n) "foo" = 'fdGetXAttr' ('Fd' n) "user.foo"@
fdGetUserXAttr :: Fd -> Name -> IO Value
fdGetUserXAttr = userXAttr fdGetXAttr


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

-- |
-- @
-- >>> 'listXAttr' "\/some\/path"
-- ["user.foo","user.bar"]
-- >>> 'listUserXAttr' "\/some\/path"
-- ["foo","bar"]
-- @
listUserXAttr :: FilePath -> IO [Name]
listUserXAttr = userXAttrList listXAttr

-- | Get the list of extended attribute @'Name'@s associated with the given
-- @'FilePath'@ in the filesystem (do not follow symbolic links).
lListXAttr :: FilePath -> IO [Name]
lListXAttr path =
    withCString path $ xAttrList llistxattr "llistxattr"

-- |
-- @
-- >>> 'lListXAttr' "\/some\/link"
-- ["user.foo","user.bar"]
-- >>> 'lListUserXAttr' "\/some\/link"
-- ["foo","bar"]
-- @
lListUserXAttr :: FilePath -> IO [Name]
lListUserXAttr = userXAttrList lListXAttr

-- | Get the list of extended attribute @'Name'@s associated with the given file
-- descriptor in the filesystem.
fdListXAttr :: Fd -> IO [Name]
fdListXAttr (Fd n) =
    xAttrList flistxattr "flistxattr" n

-- |
-- @
-- >>> 'fdListXAttr' ('Fd' n)
-- ["user.foo","user.bar"]
-- >>> 'fdListUserXAttr' ('Fd' n)
-- ["foo","bar"]
-- @
fdListUserXAttr :: Fd -> IO [Name]
fdListUserXAttr = userXAttrList fdListXAttr


xAttrRemove :: Name -> (a -> CString -> IO CInt) -> String -> a -> IO ()
xAttrRemove attr func name f =
    throwErrnoIfMinus1_ name $ withCString attr (func f)

-- | Remove the extended attribute identified by @'Name'@ and associated with
-- the given @'FilePath'@ in the filesystem, or fail with
-- @`System.IO.Error.isDoesNotExistError`@ if the attribute does not exist.
removeXAttr :: FilePath -> Name -> IO ()
removeXAttr path attr =
    withCString path $ xAttrRemove attr removexattr "removexattr"

-- | @'removeUserXAttr' "\/some\/path" "foo" = 'removeXAttr' "\/some\/path" "user.foo"@
removeUserXAttr :: FilePath -> Name -> IO ()
removeUserXAttr = userXAttr removeXAttr

-- | Remove the extended attribute identified by @'Name'@ and associated with
-- the given @'FilePath'@ in the filesystem, or fail with
-- @`System.IO.Error.isDoesNotExistError`@ if the attribute does not exist (do
-- not follow symbolic links).
lRemoveXAttr :: FilePath -> Name -> IO ()
lRemoveXAttr path attr =
    withCString path $ xAttrRemove attr lremovexattr "lremovexattr"

-- | @'lRemoveUserXAttr' "\/some\/link" "foo" = 'lRemoveXAttr' "\/some\/link" "user.foo"@
lRemoveUserXAttr :: FilePath -> Name -> IO ()
lRemoveUserXAttr = userXAttr lRemoveXAttr

-- | Remove the extended attribute identified by @'Name'@ and associated with
-- the given file descriptor in the filesystem, or fail with
-- @`System.IO.Error.isDoesNotExistError`@ if the attribute does not exist.
fdRemoveXAttr :: Fd -> Name -> IO ()
fdRemoveXAttr (Fd n) attr =
    xAttrRemove attr fremovexattr "fremovexattr" n

-- | @'fdRemoveUserXAttr' ('Fd' n) "foo" = 'fdRemoveXAttr' ('Fd' n) "user.foo"@
fdRemoveUserXAttr :: Fd -> Name -> IO ()
fdRemoveUserXAttr = userXAttr fdRemoveXAttr


userXAttr :: (a -> Name -> b) -> a -> Name -> b
userXAttr func f name = func f ("user." ++ name)

userXAttrList :: (a -> IO [Name]) -> a -> IO [Name]
userXAttrList func f = liftM unUser $ func f
    where unUser []     = []
          unUser (x:xs) = case splitAt 5 x of
                            ("user.",attr) -> attr : unUser xs
                            _              -> unUser xs


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
