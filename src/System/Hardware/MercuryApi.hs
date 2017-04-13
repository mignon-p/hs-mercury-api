{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

module System.Hardware.MercuryApi where

import Control.Applicative
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Typeable
import Data.Word
import Foreign
import Foreign.C

import System.Hardware.MercuryApi.Generated

newtype ReaderEtc = ReaderEtc ()
newtype Reader = Reader (ForeignPtr ReaderEtc)

newtype TagReadData = TagReadData ()

type RawStatus = Word32
type RawType = Word32
type RawParam = CInt

-- Many of these need to be safe because they could call back
-- into Haskell via the transport listener.

foreign import ccall unsafe "glue.h c_TMR_create"
    c_TMR_create :: Ptr ReaderEtc
                 -> CString
                 -> IO RawStatus

foreign import ccall safe "glue.h c_TMR_connect"
    c_TMR_connect :: Ptr ReaderEtc
                  -> IO RawStatus

foreign import ccall unsafe "glue.h c_TMR_destroy"
    c_TMR_destroy :: Ptr ReaderEtc
                  -> IO RawStatus

foreign import ccall safe "glue.h c_TMR_read"
    c_TMR_read :: Ptr ReaderEtc
               -> Word32
               -> Ptr Word32
               -> IO RawStatus

foreign import ccall safe "glue.h c_TMR_hasMoreTags"
    c_TMR_hasMoreTags :: Ptr ReaderEtc
                      -> IO RawStatus

foreign import ccall safe "glue.h c_TMR_getNextTag"
    c_TMR_getNextTag :: Ptr ReaderEtc
                     -> Ptr TagReadData
                     -> IO RawStatus

foreign import ccall unsafe "glue.h c_TMR_strerr"
    c_TMR_strerr :: Ptr ReaderEtc
                 -> RawStatus
                 -> IO CString

foreign import ccall unsafe "tm_reader.h TMR_paramName"
    c_TMR_paramName :: RawParam
                    -> CString

data MercuryException =
  MercuryException
  { meStatusType :: StatusType
  , meStatus     :: Status
  , meMessage    :: T.Text
  , meLocation   :: T.Text
  }
  deriving (Eq, Ord, Show, Read, Typeable)

instance Exception MercuryException

statusGetType :: RawStatus -> RawType
statusGetType stat = stat `shiftR` 24

errnoBit :: Int
errnoBit = 15

statusIsErrno :: StatusType -> RawStatus -> Bool
statusIsErrno ERROR_TYPE_COMM rstat = rstat `testBit` errnoBit
statusIsErrno _ _ = False

statusGetErrno :: RawStatus -> Errno
statusGetErrno rstat = Errno $ fromIntegral $ rstat .&. (bit errnoBit - 1)

-- I'm not sure what encoding MercuryApi uses for its strings.
-- I'm guessing UTF-8 for now, but the encoding is encapsulated in
-- these two functions (textFromBS and textToBS) so it can be
-- easily changed.
textFromBS :: B.ByteString -> T.Text
textFromBS = T.decodeUtf8With T.lenientDecode

textToBS :: T.Text -> B.ByteString
textToBS = T.encodeUtf8

textFromCString :: CString -> IO T.Text
textFromCString cs = textFromBS <$> B.packCString cs

checkStatus :: Ptr ReaderEtc -> RawStatus -> T.Text -> IO ()
checkStatus rdr rstat loc = do
  let t = toStatusType $ statusGetType rstat
      stat = toStatus rstat
  case t of
    SUCCESS_TYPE -> return ()
    _ -> do
      if statusIsErrno t rstat
        then do
        let errno = statusGetErrno rstat
            ioe = errnoToIOError (T.unpack loc) errno Nothing Nothing
        throwIO ioe
        else do
        cstr <- c_TMR_strerr rdr rstat
        msg <- textFromCString cstr
        let exc = MercuryException
                  { meStatusType = t
                  , meStatus = stat
                  , meMessage = msg
                  , meLocation = loc
                  }
        throwIO exc
