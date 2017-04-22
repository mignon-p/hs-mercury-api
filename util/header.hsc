-- This file is inserted at the top of Generated.hsc by generate-tmr-hsc.pl
{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveDataTypeable #-}
module System.Hardware.MercuryApi.Generated where

import Control.Applicative
import Control.Exception
import Data.Hashable
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Typeable
import Data.Word
import Foreign
import Foreign.C

#include <tm_reader.h>
#include <glue.h>
#include <stdbool.h>

type CBool = #{type bool}
newtype ReaderEtc = ReaderEtc ()

sizeofReaderEtc :: Int
sizeofReaderEtc = #{size ReaderEtc}

uriPtr :: Ptr ReaderEtc -> CString
uriPtr = #{ptr ReaderEtc, reader.uri}

-- I'm not sure what encoding MercuryApi uses for its strings.
-- I'm guessing UTF-8 for now, but the encoding is encapsulated in
-- these two functions (textFromBS and textToBS) so it can be
-- easily changed.
textFromBS :: ByteString -> Text
textFromBS = T.decodeUtf8With T.lenientDecode

textToBS :: Text -> ByteString
textToBS = T.encodeUtf8

textFromCString :: CString -> IO Text
textFromCString cs = textFromBS <$> B.packCString cs

-- This exception is never seen by the user.  It is caught
-- internally and turned into a MercuryException (with some added fields).
data ParamException = ParamException StatusType Status Text
  deriving (Eq, Ord, Show, Read, Typeable)

instance Exception ParamException

castLen' :: Integral a => a -> Text -> Int -> IO a
castLen' bound description x = do
  let tShow = T.pack . show
      maxLen = fromIntegral bound
  if x > maxLen
    then throwIO ( ParamException ERROR_TYPE_MISC ERROR_TOO_BIG $
                   description <> " had length " <> tShow x <>
                   " but maximum is " <> tShow maxLen )
    else return $ fromIntegral x

castLen :: (Integral a, Bounded a) => Text -> Int -> IO a
castLen = castLen' maxBound

class ParamValue a where
  pType :: a -> ParamType
  pGet :: (Ptr () -> IO ()) -> IO a
  pSet :: a -> (Ptr () -> IO ()) -> IO ()
