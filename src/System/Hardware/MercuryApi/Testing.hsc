{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
{-|
Module      : System.Hardware.MercuryApi.Testing
Description : A serial transport for MercuryApi which replays data from a file
Copyright   : © Patrick Pelletier, 2017
License     : MIT
Maintainer  : code@funwithsoftware.org

This module is not meant to be used by the end user.  It exists for the
automated tests.  It provides a handler for URIs of the form
test:///path/to/file, where the file is in the format produced by
'opcodeListener'.  When Mercury API reads from the transport, the data
from a "Received:" line is returned.  When Mercury API writes to the
transport, the data is compared to the data in the "Sending:" line.
If the data does not match, an error message is printed, and
TMR_ERROR_TIMEOUT is returned.  (Timeout was chosen because it's the only
error that seems to be consistently propagated from the transport by
Mercury API.)
-}

module System.Hardware.MercuryApi.Testing
  ( registerTransportInit
  ) where

import Control.Applicative ( Applicative((<*>)), (<$>) )
import Control.Concurrent ( threadDelay )
import Control.Exception ( IOException, try )
import Control.Monad ( when )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8 ( unpack )
import Data.Char ( isDigit )
import Data.IORef ( IORef, writeIORef, readIORef, newIORef )
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T ( encodeUtf8 )
import qualified Data.Text.IO as T ( readFile, putStrLn )
import Data.Word ( Word8, Word32 )
import Foreign
    ( newStablePtr,
      Ptr,
      FunPtr,
      nullPtr,
      Storable(alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf),
      freeStablePtr,
      deRefStablePtr,
      castStablePtrToPtr,
      castPtrToStablePtr,
      nullFunPtr,
      castPtr,
      copyArray )
import Foreign.C ( CString, withCAString, peekCAString )
import System.Info ( os )
import qualified System.IO.Unsafe as U ( unsafePerformIO )

import System.Hardware.MercuryApi hiding (read)

#include <tm_reader.h>
#include <glue.h>

type RawStatus = Word32

successStatus :: RawStatus
successStatus = #{const TMR_SUCCESS}

failureStatus :: RawStatus
failureStatus = #{const TMR_ERROR_TIMEOUT}

data SerialState =
  SerialState
  { ssFilename :: String
  , ssNext :: IORef [T.Text]
  , ssLeftover :: IORef B.ByteString
  , ssSendTime :: IORef Double
  }

data SerialTransport =
  SerialTransport
  { stCookie :: Ptr ()
  , stOpen :: FunPtr (Ptr SerialTransport -> IO RawStatus)
  , stSendBytes :: FunPtr (Ptr SerialTransport -> Word32 -> Ptr Word8 -> Word32 -> IO RawStatus)
  , stReceiveBytes :: FunPtr (Ptr SerialTransport -> Word32 -> Ptr Word32 -> Ptr Word8 -> Word32 -> IO RawStatus)
  , stSetBaudRate :: FunPtr (Ptr SerialTransport -> Word32 -> IO RawStatus)
  , stShutdown :: FunPtr (Ptr SerialTransport -> IO RawStatus)
  , stFlush :: FunPtr (Ptr SerialTransport -> IO RawStatus)
  }

instance Storable SerialTransport where
  sizeOf _ = #{size TMR_SR_SerialTransport}
  alignment _ = 8

  peek p =
    SerialTransport
    <$> #{peek TMR_SR_SerialTransport, cookie}       p
    <*> #{peek TMR_SR_SerialTransport, open}         p
    <*> #{peek TMR_SR_SerialTransport, sendBytes}    p
    <*> #{peek TMR_SR_SerialTransport, receiveBytes} p
    <*> #{peek TMR_SR_SerialTransport, setBaudRate}  p
    <*> #{peek TMR_SR_SerialTransport, shutdown}     p
    <*> #{peek TMR_SR_SerialTransport, flush}        p

  poke p x = do
    #{poke TMR_SR_SerialTransport, cookie}       p (stCookie x)
    #{poke TMR_SR_SerialTransport, open}         p (stOpen x)
    #{poke TMR_SR_SerialTransport, sendBytes}    p (stSendBytes x)
    #{poke TMR_SR_SerialTransport, receiveBytes} p (stReceiveBytes x)
    #{poke TMR_SR_SerialTransport, setBaudRate}  p (stSetBaudRate x)
    #{poke TMR_SR_SerialTransport, shutdown}     p (stShutdown x)
    #{poke TMR_SR_SerialTransport, flush}        p (stFlush x)

foreign import ccall "tm_reader.h TMR_setSerialTransport"
    c_TMR_setSerialTransport :: CString
                             -> FunPtr (Ptr SerialTransport -> Ptr () -> CString -> IO RawStatus)
                             -> IO RawStatus

foreign import ccall "wrapper"
    wrapOneArg :: (Ptr SerialTransport -> IO RawStatus)
               -> IO (FunPtr (Ptr SerialTransport -> IO RawStatus))

foreign import ccall "wrapper"
    wrapSendBytes :: (Ptr SerialTransport -> Word32 -> Ptr Word8 -> Word32 -> IO RawStatus)
                  -> IO (FunPtr (Ptr SerialTransport -> Word32 -> Ptr Word8 -> Word32 -> IO RawStatus))

foreign import ccall "wrapper"
    wrapReceiveBytes :: (Ptr SerialTransport -> Word32 -> Ptr Word32 -> Ptr Word8 -> Word32 -> IO RawStatus)
                     -> IO (FunPtr (Ptr SerialTransport -> Word32 -> Ptr Word32 -> Ptr Word8 -> Word32 -> IO RawStatus))

foreign import ccall "wrapper"
    wrapInit :: (Ptr SerialTransport -> Ptr () -> CString -> IO RawStatus)
             -> IO (FunPtr (Ptr SerialTransport -> Ptr () -> CString -> IO RawStatus))

funOpen :: FunPtr (Ptr SerialTransport -> IO RawStatus)
{-# NOINLINE funOpen #-}
funOpen = U.unsafePerformIO $ wrapOneArg testOpen

funSendBytes :: FunPtr (Ptr SerialTransport -> Word32 -> Ptr Word8 -> Word32 -> IO RawStatus)
{-# NOINLINE funSendBytes #-}
funSendBytes = U.unsafePerformIO $ wrapSendBytes testSendBytes

funReceiveBytes :: FunPtr (Ptr SerialTransport -> Word32 -> Ptr Word32 -> Ptr Word8 -> Word32 -> IO RawStatus)
{-# NOINLINE funReceiveBytes #-}
funReceiveBytes = U.unsafePerformIO $ wrapReceiveBytes testReceiveBytes

funShutdown :: FunPtr (Ptr SerialTransport -> IO RawStatus)
{-# NOINLINE funShutdown #-}
funShutdown = U.unsafePerformIO $ wrapOneArg testShutdown

funFlush :: FunPtr (Ptr SerialTransport -> IO RawStatus)
{-# NOINLINE funFlush #-}
funFlush = U.unsafePerformIO $ wrapOneArg testFlush

funTransportInit :: FunPtr (Ptr SerialTransport -> Ptr () -> CString -> IO RawStatus)
{-# NOINLINE funTransportInit #-}
funTransportInit = U.unsafePerformIO $ wrapInit testTransportInit

mkSerialTransport :: Ptr () -> SerialTransport
mkSerialTransport cookie =
  SerialTransport
  { stCookie = cookie
  , stOpen = funOpen
  , stSendBytes = funSendBytes
  , stReceiveBytes = funReceiveBytes
  , stSetBaudRate = nullFunPtr
  , stShutdown = funShutdown
  , stFlush = funFlush
  }

getState :: Ptr SerialTransport -> IO SerialState
getState p = do
  st <- peek p
  let stable = castPtrToStablePtr $ stCookie st
  deRefStablePtr stable

printIOException :: IOException -> IO ()
printIOException = print

testOpen :: Ptr SerialTransport -> IO RawStatus
testOpen p = do
  ss <- getState p
  eth <- try $ T.readFile (ssFilename ss)
  case eth of
    Left exc -> do
      printIOException exc
      return failureStatus
    Right txt -> do
      writeIORef (ssNext ss) (T.lines txt)
      return successStatus

parseTransportLine :: T.Text -> (Maybe TransportDirection, B.ByteString)
parseTransportLine txt =
  let txt' = T.takeWhile (/= '|') txt
      f d = if d == "Sending" then Tx else Rx
      (dir, mbs) = case T.splitOn ":" txt' of
                     [x] -> (Nothing, hexToBytes $ T.filter (/= ' ') x)
                     [d, x] -> (Just (f d),
                                Just $ T.encodeUtf8 $ T.dropWhile (not . isDigit) x)
                     _ -> (Nothing, Just "")
  in case mbs of
       Nothing -> (dir, "barf!")
       Just bs -> (dir, bs)

parseTransport :: [T.Text]
               -> Maybe (TransportDirection, B.ByteString, Double, [T.Text])
parseTransport [] = Nothing
parseTransport (t:ts) =
  let (Just dir, bs) = parseTransportLine t
      rest = takeWhile ((== Nothing) . fst) $ map parseTransportLine ts
      leftover = drop (length rest) ts
      bss = map snd rest
      tm = read $ B8.unpack bs
  in Just (dir, B.concat bss, tm, leftover)

takeNext :: SerialState -> IO (Maybe (TransportDirection, B.ByteString, Double))
takeNext ss = do
  let ref = ssNext ss
  ts <- readIORef ref
  let result = parseTransport ts
  case result of
    Nothing -> return Nothing
    Just (dir, bs, tm, ts') -> do
      writeIORef ref ts'
      return $ Just (dir, bs, tm)

testSendBytes :: Ptr SerialTransport
              -> Word32
              -> Ptr Word8
              -> Word32
              -> IO RawStatus
testSendBytes p len msg _ = do
  ss <- getState p
  nxt <- takeNext ss
  case nxt of
    Just (Tx, expected, tm) -> do
      writeIORef (ssSendTime ss) tm
      actual <- B.packCStringLen (castPtr msg, fromIntegral len)
      if actual == expected
        then return successStatus
        else do
        T.putStrLn ("expected <" <> bytesToHex expected <>
                    ">, but got <" <> bytesToHex actual <> ">")
        return failureStatus
    x -> do
      putStrLn $ "expected Tx, but got " ++ show x
      return failureStatus

computeDelay :: Double -> Double -> Int
computeDelay oldTime newTime =
  -- convert seconds to microseconds, and add a fudge factor of 10%
  ceiling $ (newTime - oldTime) * 1.1e6

getNextBytes :: SerialState -> IO (Either RawStatus B.ByteString)
getNextBytes ss = do
  leftover <- readIORef (ssLeftover ss)
  if B.null leftover
    then do
    nxt <- takeNext ss
    case nxt of
      Just (Rx, bs, tm) -> do
        sendTime <- readIORef (ssSendTime ss)
        threadDelay $ computeDelay sendTime tm
        return $ Right bs
      x -> do
        putStrLn $ "expected Rx, but got " ++ show x
        return $ Left failureStatus
    else return $ Right leftover

testReceiveBytes :: Ptr SerialTransport
                 -> Word32
                 -> Ptr Word32
                 -> Ptr Word8
                 -> Word32
                 -> IO RawStatus
testReceiveBytes p len returnLen msg _ = do
  ss <- getState p
  eth <- getNextBytes ss
  case eth of
    Left status -> return status
    Right bs -> do
      let (bs1, bs2) = B.splitAt (fromIntegral len) bs
      B.useAsCStringLen bs1 $ \(pChar, bsLen) -> do
        poke returnLen (fromIntegral len)
        copyArray msg (castPtr pChar) bsLen
      writeIORef (ssLeftover ss) bs2
      return successStatus

testFlush :: Ptr SerialTransport -> IO RawStatus
testFlush _ = return successStatus

testShutdown :: Ptr SerialTransport -> IO RawStatus
testShutdown p = do
  st <- peek p
  let stable = castPtrToStablePtr $ stCookie st
  freeStablePtr stable
  poke p st { stCookie = nullPtr }
  return successStatus

-- Somehow in Windows, we end up with an absolute path that
-- starts with "/C:", which doesn't work, so we need to strip
-- the leading slash.
hackPath :: String -> String -> String
hackPath "mingw32" = dropWhile (== '/')
hackPath _ = id

testTransportInit :: Ptr SerialTransport -> Ptr () -> CString -> IO RawStatus
testTransportInit p _ cstr = do
  fname <- hackPath os <$> peekCAString cstr
  ref <- newIORef []
  ref2 <- newIORef ""
  ref3 <- newIORef 0
  let ss = SerialState fname ref ref2 ref3
  stable <- newStablePtr ss
  let st = mkSerialTransport $ castStablePtrToPtr stable
  poke p st
  return successStatus

registerTransportInit :: IO ()
registerTransportInit = do
  withCAString "test" $ \name -> do
    status <- c_TMR_setSerialTransport name funTransportInit
    when (status /= successStatus) $ fail "TMR_setSerialTransport failed"
