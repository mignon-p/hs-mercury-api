{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, OverloadedStrings #-}

module System.Hardware.MercuryApi
  ( Reader
  , Param (..)
  , MercuryException (..)
  , StatusType (..)
  , Status (..)
  , TransportListener
  , TransportListenerId
  , create
  , connect
  , destroy
  , paramSet
  , paramGet
  , paramList
  , paramName
  , paramID
  , addTransportListener
  , removeTransportListener
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Typeable
import Data.Word
import Foreign
import Foreign.C
import qualified System.IO.Unsafe as U

import System.Hardware.MercuryApi.Generated

newtype Reader = Reader (ForeignPtr ReaderEtc)

newtype TagReadData = TagReadData ()

type RawStatus = Word32
type RawType = Word32

cFalse, cTrue :: CBool
cFalse = 0
cTrue = 1

type RawTransportListener =
  CBool -> Word32 -> Ptr Word8 -> Word32 -> Ptr () -> IO ()

type TransportListener = Bool -> B.ByteString -> Word32 -> IO ()

newtype TransportListenerId = TransportListenerId Integer

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

foreign import ccall safe "glue.h c_TMR_paramSet"
    c_TMR_paramSet :: Ptr ReaderEtc
                   -> RawParam
                   -> Ptr ()
                   -> IO RawStatus

foreign import ccall safe "glue.h c_TMR_paramGet"
    c_TMR_paramGet :: Ptr ReaderEtc
                   -> RawParam
                   -> Ptr ()
                   -> IO RawStatus

foreign import ccall safe "glue.h c_TMR_paramList"
    c_TMR_paramList :: Ptr ReaderEtc
                    -> Ptr RawParam
                    -> Ptr Word32
                    -> IO RawStatus

foreign import ccall "wrapper"
    wrapTransportListener :: RawTransportListener
                          -> IO (FunPtr RawTransportListener)

-- takes ownership of the FunPtr and the CString
foreign import ccall unsafe "glue.h c_TMR_addTransportListener"
    c_TMR_addTransportListener :: Ptr ReaderEtc
                               -> FunPtr RawTransportListener
                               -> CString
                               -> IO RawStatus

foreign import ccall unsafe "glue.h c_TMR_removeTransportListener"
    c_TMR_removeTransportListener :: Ptr ReaderEtc
                                  -> CString
                                  -> IO RawStatus

foreign import ccall unsafe "glue.h c_TMR_strerr"
    c_TMR_strerr :: Ptr ReaderEtc
                 -> RawStatus
                 -> IO CString

-- This is a pure function, because it returns a string literal
foreign import ccall unsafe "tm_reader.h TMR_paramName"
    c_TMR_paramName :: RawParam
                    -> CString

-- | Represents any error that can occur in a MercuryApi call,
-- except for those which can be represented by 'IOException'.
data MercuryException =
  MercuryException
  { meStatusType :: StatusType -- ^ general category of error
  , meStatus     :: Status     -- ^ the specific error
  , meMessage    :: T.Text     -- ^ the error message
  , meLocation   :: T.Text     -- ^ function where the error occurred
  , meParam      :: T.Text     -- ^ more information, such as the parameter
                               -- in 'paramGet' or 'paramSet'
  , meUri        :: T.Text     -- ^ URI of the reader
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

checkStatus' :: Ptr ReaderEtc
             -> RawStatus
             -> T.Text
             -> T.Text
             -> IO T.Text
             -> IO ()
checkStatus' rdr rstat loc param getUri = do
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
        uri <- getUri
        let exc = MercuryException
                  { meStatusType = t
                  , meStatus = stat
                  , meMessage = msg
                  , meLocation = loc
                  , meParam = param
                  , meUri = uri
                  }
        throwIO exc

checkStatus :: Ptr ReaderEtc -> RawStatus -> T.Text -> T.Text -> IO ()
checkStatus rdr rstat loc param =
  checkStatus' rdr rstat loc param (textFromCString $ uriPtr rdr)

uniqueCounter :: IORef Integer
uniqueCounter = U.unsafePerformIO $ newIORef 0

newUnique :: IO Integer
newUnique = atomicModifyIORef' uniqueCounter f
  where f x = (x + 1, x)

castToCStringLen :: Integral a => a -> Ptr Word8 -> CStringLen
castToCStringLen len ptr = (castPtr ptr, fromIntegral len)

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
      checkStatus' p status "create" "" (return deviceUri)
    addForeignPtrFinalizer p_TMR_destroy fp
    return $ Reader fp

withReaderEtc :: Reader
              -> T.Text
              -> T.Text
              -> (Ptr ReaderEtc -> IO RawStatus)
              -> IO ()
withReaderEtc (Reader fp) location param func = do
  withForeignPtr fp $ \p -> do
    status <- func p
    checkStatus p status location param

-- | Establishes the connection to the reader at the URI specified in
-- the 'create' call.  The existence of a reader at the address is
-- verified and the reader is brought into a state appropriate for
-- performing RF operations.
connect :: Reader -> IO ()
connect rdr = withReaderEtc rdr "connect" "" c_TMR_connect

-- | Closes the connection to the reader and releases any resources
-- that have been consumed by the reader structure.
destroy :: Reader -> IO ()
destroy rdr = withReaderEtc rdr "destroy" "" c_TMR_destroy

throwBinding :: Reader -> (Status, T.Text) -> T.Text -> T.Text -> IO ()
throwBinding (Reader fp) (status, msg) loc param = do
  uri <- withForeignPtr fp (textFromCString . uriPtr)
  throwIO $ MercuryException
    { meStatusType = ERROR_TYPE_BINDING
    , meStatus = status
    , meMessage = msg
    , meLocation = loc
    , meParam = param
    , meUri = uri
    }

unimplementedParam :: (Status, T.Text)
unimplementedParam =
  ( ERROR_UNIMPLEMENTED_PARAM
  , "The given parameter is not yet implemented in the Haskell binding."
  )

invalidParam :: ParamType -> ParamType -> (Status, T.Text)
invalidParam expected actual =
  ( ERROR_INVALID_PARAM_TYPE
  , "Expected " <> paramTypeDisplay expected <>
    " but got " <> paramTypeDisplay actual
  )

paramSet :: ParamValue a => Reader -> Param -> a -> IO ()
paramSet rdr param value = do
  let pt = paramType param
      pt' = pType value
      rp = fromParam param
      pName = T.pack $ show param
  when (pt == ParamTypeUnimplemented) $
    throwBinding rdr unimplementedParam "paramSet" pName
  when (pt /= pt') $
    throwBinding rdr (invalidParam pt pt') "paramSet" pName
  pSet value $ \pp -> withReaderEtc rdr "paramSet" pName $
                      \p -> c_TMR_paramSet p rp pp

withReturnType :: (a -> IO a) -> IO a
withReturnType f = f undefined

paramGet :: ParamValue a => Reader -> Param -> IO a
paramGet rdr param = withReturnType $ \returnType -> do
  let pt = paramType param
      pt' = pType returnType
      rp = fromParam param
      pName = T.pack $ show param
  when (pt == ParamTypeUnimplemented) $
    throwBinding rdr unimplementedParam "paramGet" pName
  when (pt /= pt') $
    throwBinding rdr (invalidParam pt pt') "paramGet" pName
  value <- pGet $ \pp -> withReaderEtc rdr "paramGet" pName $
                         \p -> c_TMR_paramGet p rp pp
  return value

-- | Get a list of parameters supported by the reader.
paramList :: Reader -> IO [Param]
paramList rdr = do
  let maxParams = paramMax + 1
  alloca $ \nParams -> do
    poke nParams (fromIntegral maxParams)
    allocaArray (fromIntegral maxParams) $ \params -> do
      withReaderEtc rdr "paramList" "" $ \p -> c_TMR_paramList p params nParams
      actual <- peek nParams
      result <- peekArray (min (fromIntegral actual) (fromIntegral maxParams)) params
      return $ map toParam result

callTransportListener :: TransportListener -> RawTransportListener
callTransportListener listener tx dataLen dataPtr timeout _ = do
  bs <- B.packCStringLen (castToCStringLen dataLen dataPtr)
  listener (toBool tx) bs timeout

-- | Add a listener to the list of functions that will be called for
-- each message sent to or recieved from the reader.
addTransportListener :: Reader                 -- ^ The reader to operate on.
                     -> TransportListener      -- ^ The listener to call.
                     -> IO TransportListenerId -- ^ A unique identifier which
                                               -- can be used to remove the
                                               -- listener later.
addTransportListener rdr listener = do
  unique <- newUnique
  funPtr <- wrapTransportListener (callTransportListener listener)
  cs <- newCAString (show unique)
  withReaderEtc rdr "addTransportListener" "" $
    \p -> c_TMR_addTransportListener p funPtr cs
  return (TransportListenerId unique)

-- | Remove a listener from the list of functions that will be called
-- for each message sent to or recieved from the reader.
removeTransportListener :: Reader              -- ^ The reader to operate on.
                        -> TransportListenerId -- ^ The return value of a call
                                               -- to 'addTransportListener'.
                        -> IO ()
removeTransportListener rdr (TransportListenerId unique) = do
  withCAString (show unique) $ \cs -> do
    withReaderEtc rdr "removeTransportListener" "" $
      \p -> c_TMR_removeTransportListener p cs
