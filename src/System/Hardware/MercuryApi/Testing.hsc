{-# LANGUAGE OverloadedStrings #-}

module System.Hardware.MercuryApi.Testing where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.IORef
import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as T
import Data.Word
import Foreign
import Foreign.C
import System.IO
import qualified System.IO.Unsafe as U

import System.Hardware.MercuryApi

#include <tm_reader.h>
#include <glue.h>

type RawStatus = Word32

successStatus :: RawStatus
successStatus = #{const TMR_SUCCESS}

failureStatus :: RawStatus
failureStatus = #{const ERROR_TEST_FAILURE}

data SerialState =
  SerialState
  { ssFilename :: String
  , ssNext :: IORef [T.Text]
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
      (dir, hex) = case T.splitOn ":" txt' of
                     [x] -> (Nothing, x)
                     [d, x] -> (Just (f d), x)
                     _ -> (Nothing, "")
      mbs = hexToBytes hex
  in case mbs of
       Nothing -> (dir, "")
       Just bs -> (dir, bs)

parseTransport :: [T.Text] -> Maybe (TransportDirection, B.ByteString, [T.Text])
parseTransport [] = Nothing
parseTransport (t:ts) =
  let (Just dir, bs) = parseTransportLine t
      rest = takeWhile ((== Nothing) . fst) $ map parseTransportLine ts
      leftover = drop (length rest) ts
      bss = bs : map snd rest
  in Just (dir, B.concat bss, leftover)

takeNext :: SerialState -> IO (Maybe (TransportDirection, B.ByteString))
takeNext ss = do
  let ref = ssNext ss
  ts <- readIORef ref
  let result = parseTransport ts
  case result of
    Nothing -> return Nothing
    Just (dir, bs, ts') -> do
      writeIORef ref ts'
      return $ Just (dir, bs)

testSendBytes :: Ptr SerialTransport
              -> Word32
              -> Ptr Word8
              -> Word32
              -> IO RawStatus
testSendBytes p len msg _ = do
  ss <- getState p
  nxt <- takeNext ss
  case nxt of
    Just (Tx, expected) -> do
      actual <- B.packCStringLen (castPtr msg, fromIntegral len)
      if actual == expected
        then return successStatus
        else do
        T.putStrLn ("expected " <> bytesToHex expected <>
                    ", but got " <> bytesToHex actual)
        return failureStatus
    x -> do
      putStrLn $ "expected Tx, but got " ++ show x
      return failureStatus

testReceiveBytes :: Ptr SerialTransport
                 -> Word32
                 -> Ptr Word32
                 -> Ptr Word8
                 -> Word32
                 -> IO RawStatus
testReceiveBytes p len returnLen msg _ = do
  ss <- getState p
  nxt <- takeNext ss
  case nxt of
    Just (Rx, bs) -> do
      B.useAsCStringLen bs $ \(pChar, bsLen) -> do
        if (fromIntegral len < bsLen)
          then do
          putStrLn ("needed " ++ show bsLen ++ " bytes, but only " ++
                    show len ++ " available")
          return failureStatus
          else do
          poke returnLen (fromIntegral len)
          copyArray msg (castPtr pChar) bsLen
          return successStatus
    x -> do
      putStrLn $ "expected Rx, but got " ++ show x
      return failureStatus

testFlush :: Ptr SerialTransport -> IO RawStatus
testFlush _ = return successStatus

testShutdown :: Ptr SerialTransport -> IO RawStatus
testShutdown p = do
  st <- peek p
  let stable = castPtrToStablePtr $ stCookie st
  freeStablePtr stable
  poke p st { stCookie = nullPtr }
  return successStatus

testTransportInit :: Ptr SerialTransport -> Ptr () -> CString -> IO RawStatus
testTransportInit p _ cstr = do
  fname <- peekCAString cstr
  ref <- newIORef []
  let ss = SerialState fname ref
  stable <- newStablePtr ss
  let st = mkSerialTransport $ castStablePtrToPtr stable
  poke p st
  return successStatus
