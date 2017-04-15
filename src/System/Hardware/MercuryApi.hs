{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, OverloadedStrings #-}

module System.Hardware.MercuryApi
  ( Reader
  , Param (..)
  , MercuryException (..)
  , StatusType (..)
  , Status (..)
  , create
  , connect
  , destroy
  , paramList
  , paramName
  , paramID
  ) where

import Control.Applicative
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Typeable
import Data.Word
import Foreign
import Foreign.C
import qualified System.IO.Unsafe as U

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

foreign import ccall unsafe "glue.h &c_TMR_destroy"
    p_TMR_destroy :: FunPtr (Ptr ReaderEtc -> IO ())

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

foreign import ccall safe "glue.h c_TMR_paramList"
    c_TMR_paramList :: Ptr ReaderEtc
                    -> Ptr RawParam
                    -> Ptr Word32
                    -> IO RawStatus

foreign import ccall unsafe "glue.h c_TMR_strerr"
    c_TMR_strerr :: Ptr ReaderEtc
                 -> RawStatus
                 -> IO CString

-- This is a pure function, because it returns a string literal
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

paramPairs :: [(Param, T.Text)]
paramPairs = map f [minBound..maxBound]
  where
    f p = (p, U.unsafePerformIO $ textFromCString $ c_TMR_paramName $ fromParam p)

paramMap :: H.HashMap Param T.Text
paramMap = H.fromList paramPairs

paramMapReverse :: H.HashMap T.Text Param
paramMapReverse = H.fromList $ map swap paramPairs
  where swap (x, y) = (y, x)

-- | Return the string name (e. g. \"\/reader\/read\/plan\")
-- corresponding to a 'Param'.
paramName :: Param -> T.Text
paramName p = paramMap H.! p -- all possible keys are in the map, so can't fail

-- | Return the 'Param' corresponding to a string name
-- (e. g. \"\/reader\/read\/plan\").  Returns 'PARAM_NONE' if no such
-- parameter exists.
paramID :: T.Text -> Param
paramID name = H.lookupDefault PARAM_NONE name paramMapReverse

-- | Create a new 'Reader' with the specified URI.  The reader is
-- not contacted at this point.
create :: T.Text -- ^ a reader URI, such as @tmr:\/\/\/dev\/ttyUSB0@
       -> IO Reader
create deviceUri = do
  B.useAsCString (textToBS deviceUri) $ \cs -> do
    fp <- mallocForeignPtrBytes sizeofReaderEtc
    withForeignPtr fp $ \p -> do
      status <- c_TMR_create p cs
      checkStatus p status "create"
    addForeignPtrFinalizer p_TMR_destroy fp
    return $ Reader fp

withReaderEtc :: Reader -> T.Text -> (Ptr ReaderEtc -> IO RawStatus) -> IO ()
withReaderEtc (Reader fp) location func = do
  withForeignPtr fp $ \p -> do
    status <- func p
    checkStatus p status location

-- | Establishes the connection to the reader at the URI specified in
-- the 'create' call.  The existence of a reader at the address is
-- verified and the reader is brought into a state appropriate for
-- performing RF operations.
connect :: Reader -> IO ()
connect rdr = withReaderEtc rdr "connect" c_TMR_connect

-- | Closes the connection to the reader and releases any resources
-- that have been consumed by the reader structure.
destroy :: Reader -> IO ()
destroy rdr = withReaderEtc rdr "destroy" c_TMR_destroy

-- | Get a list of parameters supported by the reader.
paramList :: Reader -> IO [Param]
paramList rdr = do
  let maxParams = paramMax + 1
  alloca $ \nParams -> do
    poke nParams (fromIntegral maxParams)
    allocaArray (fromIntegral maxParams) $ \params -> do
      withReaderEtc rdr "paramList" $ \p -> c_TMR_paramList p params nParams
      actual <- peek nParams
      result <- peekArray (min (fromIntegral actual) (fromIntegral maxParams)) params
      return $ map toParam result
