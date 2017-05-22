-- This file is inserted at the top of Generated.hsc by generate-tmr-hsc.pl
{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveDataTypeable #-}
module System.Hardware.MercuryApi.Generated where

import Control.Applicative
import Control.Exception
import Data.Hashable
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Typeable
import Data.Word
import Foreign
import Foreign.C

import System.Hardware.MercuryApi.Enums

#include <tm_reader.h>
#include <glue.h>
#include <stdbool.h>

-- | A GPIO pin number.  On the M6e Nano, these are numbered 1-4.
type PinNumber = Word8

-- | An antenna number.  On the
-- <https://www.sparkfun.com/products/14066 SparkFun Simultaneous RFID Reader>,
-- there is a single antenna with the number 1.
type AntennaPort = Word8

-- | A 32-bit password (access or kill) in the Gen2 protocol.
type GEN2_Password = Word32

-- | milliseconds since 1\/1\/1970 UTC
type MillisecondsSinceEpoch = Word64

-- | Version number of the Mercury API C library.
apiVersion :: Text
apiVersion = #{const_str TMR_VERSION}

type CBool = #{type bool}
newtype ReaderEtc = ReaderEtc ()

cFalse, cTrue :: CBool
cFalse = 0
cTrue = 1

toBool' :: CBool -> Bool
toBool' = toBool

fromBool' :: Bool -> CBool
fromBool' = fromBool

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

-- | A ReadPlan record specifies the antennas, protocols, and filters
-- to use for a search (read).
--
-- Currently, only @SimpleReadPlan@ is supported.
data ReadPlan =
  SimpleReadPlan
  { rpWeight        :: !Word32          -- ^ The relative weight of this read plan
  , rpEnableAutonomousRead :: !Bool     -- ^ Option for Autonomous read
  , rpAntennas      :: ![AntennaPort]   -- ^ The list of antennas to read on
  , rpProtocol      :: !TagProtocol     -- ^ The protocol to use for reading
  , rpFilter        :: !(Maybe TagFilter) -- ^ The filter to apply to reading
  , rpTagop         :: !(Maybe TagOp)   -- ^ The tag operation to apply to
                                        -- each read tag
  , rpUseFastSearch :: !Bool            -- ^ Option to use the FastSearch
  , rpStopOnCount   :: !(Maybe Word32)  -- ^ Number of tags to be read
  , rpTriggerRead   :: !(Maybe [Word8]) -- ^ The list of GPI ports which should
                                        -- be used to trigger the read
  } deriving (Eq, Ord, Show, Read)

antennasInfo :: Ptr ReadPlan -> (Ptr List16, Word16, Ptr Word8, Text)
antennasInfo rp =
  ( #{ptr ReadPlanEtc, plan.u.simple.antennas} rp
  , #{const GLUE_MAX_ANTENNAS}
  , #{ptr ReadPlanEtc, antennas} rp
  , "rpAntennas"
  )

gpiListInfo :: Ptr ReadPlan -> (Ptr List16, Word16, Ptr Word8, Text)
gpiListInfo rp =
  ( #{ptr ReadPlanEtc, plan.u.simple.triggerRead.gpiList} rp
  , #{const GLUE_MAX_GPIPORTS}
  , #{ptr ReadPlanEtc, gpiPorts} rp
  , "rpTriggerRead"
  )

readPlanTypeSimple :: #{type TMR_ReadPlanType}
readPlanTypeSimple = #{const TMR_READ_PLAN_TYPE_SIMPLE}

instance Storable ReadPlan where
  sizeOf _ = #{size ReadPlanEtc}
  alignment _ = 8

  poke p x = do
    #{poke ReadPlanEtc, plan.type} p readPlanTypeSimple
    #{poke ReadPlanEtc, plan.weight} p (rpWeight x)
    #{poke ReadPlanEtc, plan.enableAutonomousRead} p
      (fromBool' $ rpEnableAutonomousRead x)
    pokeList16 (antennasInfo p) (rpAntennas x)
    #{poke ReadPlanEtc, plan.u.simple.protocol} p
      (fromTagProtocol $ rpProtocol x)
    case rpFilter x of
      Nothing -> #{poke ReadPlanEtc, plan.u.simple.filter} p nullPtr
      Just f -> do
        #{poke ReadPlanEtc, filter} p f
        #{poke ReadPlanEtc, plan.u.simple.filter} p (#{ptr ReadPlanEtc, filter} p)
    case rpTagop x of
      Nothing -> #{poke ReadPlanEtc, plan.u.simple.tagop} p nullPtr
      Just op -> do
        #{poke ReadPlanEtc, tagop} p op
        #{poke ReadPlanEtc, plan.u.simple.tagop} p (#{ptr ReadPlanEtc, tagop} p)
    #{poke ReadPlanEtc, plan.u.simple.useFastSearch} p
      (fromBool' $ rpUseFastSearch x)
    let (stop, nTags) = case rpStopOnCount x of
                          Nothing -> (cFalse, 0)
                          Just n -> (cTrue, n)
    #{poke ReadPlanEtc, plan.u.simple.stopOnCount.stopNTriggerStatus} p stop
    #{poke ReadPlanEtc, plan.u.simple.stopOnCount.noOfTags} p nTags
    let (enable, ports) = case rpTriggerRead x of
                            Nothing -> (cFalse, [])
                            Just ps -> (cTrue, ps)
    #{poke ReadPlanEtc, plan.u.simple.triggerRead.enable} p enable
    pokeList16 (gpiListInfo p) ports

  peek p = do
    weight <- #{peek ReadPlanEtc, plan.weight} p
    enableAutonomousRead <- #{peek ReadPlanEtc, plan.enableAutonomousRead} p
    antennas <- peekList16 (antennasInfo p)
    protocol <- #{peek ReadPlanEtc, plan.u.simple.protocol} p
    fPtr <- #{peek ReadPlanEtc, plan.u.simple.filter} p
    filt <- if fPtr == nullPtr
            then return Nothing
            else Just <$> peek fPtr
    opPtr <- #{peek ReadPlanEtc, plan.u.simple.tagop} p
    op <- if opPtr == nullPtr
          then return Nothing
          else Just <$> peek opPtr
    useFastSearch <- #{peek ReadPlanEtc, plan.u.simple.useFastSearch} p
    stop <- #{peek ReadPlanEtc, plan.u.simple.stopOnCount.stopNTriggerStatus} p
    stopOnCount <- if toBool' stop
                   then Just <$> #{peek ReadPlanEtc, plan.u.simple.stopOnCount.noOfTags} p
                   else return Nothing
    enable <- #{peek ReadPlanEtc, plan.u.simple.triggerRead.enable} p
    triggerRead <- if toBool' enable
                   then Just <$> peekList16 (gpiListInfo p)
                   else return Nothing
    return $ SimpleReadPlan
      { rpWeight = weight
      , rpEnableAutonomousRead = toBool' enableAutonomousRead
      , rpAntennas = antennas
      , rpProtocol = toTagProtocol protocol
      , rpFilter = filt
      , rpTagop = op
      , rpUseFastSearch = toBool' useFastSearch
      , rpStopOnCount = stopOnCount
      , rpTriggerRead = triggerRead
      }

-- | Filter on EPC length, or on a Gen2 bank.
data FilterOn = FilterOnBank GEN2_Bank
              | FilterOnEpcLength
              deriving (Eq, Ord, Show, Read)

instance Storable FilterOn where
  sizeOf _ = #{size TMR_GEN2_Bank}
  alignment _ = 8

  poke p FilterOnEpcLength = do
    let p' = castPtr p :: Ptr RawBank
    poke p' #{const TMR_GEN2_EPC_LENGTH_FILTER}

  poke p (FilterOnBank bank) = do
    let p' = castPtr p :: Ptr RawBank
    poke p' (fromBank bank)

  peek p = do
    x <- peek (castPtr p)
    if x == #{const TMR_GEN2_EPC_LENGTH_FILTER}
      then return FilterOnEpcLength
      else return $ FilterOnBank $ toBank x

-- | Filter on EPC data, or on Gen2-specific information.
data TagFilter = TagFilterEPC TagData
               | TagFilterGen2
               { tfInvert        :: !Bool        -- ^ Whether to invert the
                                                 -- selection (deselect tags
                                                 -- that meet the comparison)
               , tfFilterOn      :: !FilterOn    -- ^ The memory bank in which
                                                 -- to compare the mask
               , tfBitPointer    :: !Word32      -- ^ The location (in bits) at
                                                 -- which to begin comparing
                                                 -- the mask
               , tfMaskBitLength :: !Word16      -- ^ The length (in bits) of
                                                 -- the mask
               , tfMask          :: !ByteString  -- ^ The mask value to compare
                                                 -- with the specified region
                                                 -- of tag memory, MSB first
               }
               deriving (Eq, Ord, Show, Read)

instance Storable TagFilter where
  sizeOf _ = #{size TagFilterEtc}
  alignment _ = 8

  poke p (TagFilterEPC td) = do
    #{poke TagFilterEtc, filter.type} p
      (#{const TMR_FILTER_TYPE_TAG_DATA} :: #{type TMR_FilterType})
    #{poke TagFilterEtc, filter.u.tagData} p td

  poke p tf@(TagFilterGen2 {}) = do
    #{poke TagFilterEtc, filter.type} p
      (#{const TMR_FILTER_TYPE_GEN2_SELECT} :: #{type TMR_FilterType})
    #{poke TagFilterEtc, filter.u.gen2Select.invert} p (fromBool' $ tfInvert tf)
    #{poke TagFilterEtc, filter.u.gen2Select.bank} p (tfFilterOn tf)
    #{poke TagFilterEtc, filter.u.gen2Select.bitPointer} p (tfBitPointer tf)
    #{poke TagFilterEtc, filter.u.gen2Select.maskBitLength} p (tfMaskBitLength tf)
    let maskLenBytes = fromIntegral $ (tfMaskBitLength tf + 7) `div` 8
        origLen = B.length (tfMask tf)
        bs = if origLen < maskLenBytes
               then tfMask tf <> B.pack (replicate (maskLenBytes - origLen) 0)
               else tfMask tf
    B.useAsCStringLen bs $ \(cs, len) -> do
      len' <- castLen' #{const GLUE_MAX_MASK} "tfMask" len
      copyArray (#{ptr TagFilterEtc, mask} p) cs (fromIntegral len')
    #{poke TagFilterEtc, filter.u.gen2Select.mask} p (#{ptr TagFilterEtc, mask} p)

  peek p = do
    ft <- #{peek TagFilterEtc, filter.type} p :: IO #{type TMR_FilterType}
    case ft of
      #{const TMR_FILTER_TYPE_TAG_DATA} ->
        TagFilterEPC <$> #{peek TagFilterEtc, filter.u.tagData} p
      #{const TMR_FILTER_TYPE_GEN2_SELECT} ->
        TagFilterGen2
        <$> (toBool' <$> #{peek TagFilterEtc, filter.u.gen2Select.invert} p)
        <*> #{peek TagFilterEtc, filter.u.gen2Select.bank} p
        <*> #{peek TagFilterEtc, filter.u.gen2Select.bitPointer} p
        <*> #{peek TagFilterEtc, filter.u.gen2Select.maskBitLength} p
        <*> peekMask p

peekMask :: Ptr TagFilter -> IO ByteString
peekMask p = do
  bitLength <- #{peek TagFilterEtc, filter.u.gen2Select.maskBitLength} p :: IO Word32
  let len = fromIntegral $ (bitLength + 7) `div` 8
  maskPtr <- #{peek TagFilterEtc, filter.u.gen2Select.mask} p
  B.packCStringLen (maskPtr, len)

packBits :: Num b => (a -> b) -> [a] -> b
packBits from flags = sum $ map from flags

unpackBits :: (Bounded a, Enum a, Num b, Bits b) => (a -> b) -> b -> [a]
unpackBits from x = mapMaybe f [minBound..maxBound]
  where f flag = if (x .&. from flag) == 0
                 then Nothing
                 else Just flag

packFlags :: [MetadataFlag] -> RawMetadataFlag
packFlags = packBits fromMetadataFlag

unpackFlags :: RawMetadataFlag -> [MetadataFlag]
unpackFlags = unpackBits fromMetadataFlag

packFlags16 :: [MetadataFlag] -> Word16
packFlags16 = fromIntegral . packFlags

unpackFlags16 :: Word16 -> [MetadataFlag]
unpackFlags16 = unpackFlags . fromIntegral

packExtraBanks :: [GEN2_Bank] -> RawBank
packExtraBanks = packBits fromExtraBank

unpackExtraBanks :: RawBank -> [GEN2_Bank]
unpackExtraBanks = unpackBits fromExtraBank

packLockBits :: [GEN2_LockBits] -> RawLockBits
packLockBits = packBits fromLockBits

unpackLockBits :: RawLockBits -> [GEN2_LockBits]
unpackLockBits = unpackBits fromLockBits

packLockBits16 :: [GEN2_LockBits] -> Word16
packLockBits16 = fromIntegral . packLockBits

unpackLockBits16 :: Word16 -> [GEN2_LockBits]
unpackLockBits16 = unpackLockBits . fromIntegral

peekArrayAsByteString :: Ptr Word8 -> Ptr Word8 -> IO ByteString
peekArrayAsByteString arrayPtr lenPtr = do
  len <- peek lenPtr
  B.packCStringLen (castPtr arrayPtr, fromIntegral len)

pokeArrayAsByteString :: Text
                      -> Word8
                      -> Ptr Word8
                      -> Ptr Word8
                      -> ByteString
                      -> IO ()
pokeArrayAsByteString desc maxLen arrayPtr lenPtr bs = do
  B.useAsCStringLen bs $ \(cs, len) -> do
    len' <- castLen' maxLen desc len
    copyArray arrayPtr (castPtr cs) (fromIntegral len')
    poke lenPtr len'

peekListAsByteString :: Ptr List16 -> IO ByteString
peekListAsByteString listPtr = do
  lst <- peek listPtr
  B.packCStringLen (castPtr $ l16_list lst, fromIntegral $ l16_len lst)

peekArrayAsList :: Storable a => Ptr a -> Ptr Word8 -> IO [a]
peekArrayAsList arrayPtr lenPtr = do
  len <- peek lenPtr
  peekArray (fromIntegral len) arrayPtr

peekListAsList :: Storable a => Ptr List16 -> Ptr a -> IO [a]
peekListAsList listPtr _ = do
  lst <- peek listPtr
  peekArray (fromIntegral $ l16_len lst) (castPtr $ l16_list lst)

pokeListAsList :: Storable a
               => Text
               -> Word16
               -> Ptr List16
               -> Ptr a
               -> [a]
               -> IO ()
pokeListAsList desc maxLen listPtr storage xs = do
  withArrayLen xs $ \len tmpPtr -> do
    len' <- castLen' maxLen desc len
    copyArray storage tmpPtr len
    let lst = List16
              { l16_list = castPtr storage
              , l16_max = maxLen
              , l16_len = len'
              }
    poke listPtr lst

peekMaybe :: (Storable a, Storable b)
          => (Ptr a -> IO a)
          -> (b -> Bool)
          -> Ptr a
          -> Ptr b
          -> IO (Maybe a)
peekMaybe oldPeek cond justP condP = do
  c <- peek condP
  if cond c
    then Just <$> oldPeek justP
    else return Nothing

pokeGen2TagData :: Ptr GEN2_TagData
                -> Ptr RawTagProtocol
                -> Maybe GEN2_TagData
                -> IO ()
pokeGen2TagData pGen2 _ mGen2 = do
  let gen2 = fromMaybe (GEN2_TagData B.empty) mGen2
  poke pGen2 gen2

peekSplit64 :: Ptr Word32 -> Ptr Word32 -> IO Word64
peekSplit64 pLow pHigh = do
  lo <- fromIntegral <$> peek pLow
  hi <- fromIntegral <$> peek pHigh
  return $ lo .|. (hi `shiftL` 32)

peekPtr :: Storable a => Ptr (Ptr a) -> Ptr a -> IO a
peekPtr pp _ = do
  p <- peek pp
  peek p

pokePtr :: Storable a => Ptr (Ptr a) -> Ptr a -> a -> IO ()
pokePtr pp p x = do
  poke p x
  poke pp p

pokeOr :: (Storable a, Bits a) => Ptr a -> a -> IO ()
pokeOr p x = do
  old <- peek p
  poke p (x .|. old)
