-- This file is inserted at the top of Generated.hsc by generate-tmr-hsc.pl
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module System.Hardware.MercuryApi.Generated where

import Control.Applicative
import Data.Hashable
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
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

type ErrorTriple = (StatusType, Status, Text)

withReturnType' :: (b -> Either a b) -> Either a b
withReturnType' f = f undefined

castLen :: (Integral a, Bounded a)
        => Text
        -> Int
        -> Either (StatusType, Status, Text) a
castLen listType x = withReturnType' cl
  where
    tShow = T.pack . show
    cl rt =
      let maxLen = fromIntegral $ maxBound `asTypeOf` rt
      in if x > maxLen
         then Left ( ERROR_TYPE_MISC
                   , ERROR_TOO_BIG
                   , listType <> " had length " <> tShow x <>
                     " but maximum is " <> tShow maxLen
                   )
         else Right $ fromIntegral x
