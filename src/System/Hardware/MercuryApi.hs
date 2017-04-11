{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

module System.Hardware.MercuryApi where

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

data MercuryException =
  MercuryException
  { meStatusType :: StatusType
  , meStatus     :: Status
  , meMessage    :: T.Text
  , meLocation   :: T.Text
  }
  deriving (Eq, Ord, Show, Read, Typeable)

instance Exception MercuryException

type RawStatus = Word32
type RawType = Word32

statusGetType :: RawStatus -> RawType
statusGetType stat = stat `shiftR` 24

errnoBit :: Int
errnoBit = 15

statusIsErrno :: StatusType -> RawStatus -> Bool
statusIsErrno ERROR_TYPE_COMM rstat = rstat `testBit` errnoBit
statusIsErrno _ _ = False

statusGetErrno :: RawStatus -> Errno
statusGetErrno rstat = Errno $ fromIntegral $ rstat .&. (bit errnoBit - 1)

textFromCString :: CString -> IO T.Text
textFromCString cs = do
  bs <- B.packCString cs
  return $ T.decodeUtf8With T.lenientDecode bs

type RawReader = () -- FIXME

foreign import ccall unsafe "tm_reader.h TMR_strerr"
    c_TMR_strerr :: Ptr RawReader
                 -> RawStatus
                 -> IO CString

checkStatus :: Ptr RawReader -> RawStatus -> T.Text -> IO ()
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
