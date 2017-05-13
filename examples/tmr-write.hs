{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Int
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word
import qualified System.Hardware.MercuryApi as TMR
import qualified System.Hardware.MercuryApi.Params as TMR
import System.IO

emptyUserDataFilter :: TMR.TagFilter
emptyUserDataFilter = TMR.TagFilterGen2
  { TMR.tfInvert = False
  , TMR.tfFilterOn = TMR.FilterOnBank TMR.GEN2_BANK_USER
  , TMR.tfBitPointer = 0
  , TMR.tfMaskBitLength = 16
  , TMR.tfMask = B.pack [0, 0]
  }

main = do
  rdr <- TMR.create "tmr:///dev/ttyUSB0"
  listener <- TMR.hexListener stdout
  TMR.addTransportListener rdr listener
  TMR.paramSet rdr TMR.PARAM_TRANSPORTTIMEOUT (10000 :: Word32)
  TMR.connect rdr

  TMR.paramSetBasics rdr TMR.REGION_NA2 500 [1]
  TMR.paramSetTagReadDataRecordHighestRssi rdr True
  TMR.paramSetReadPlanFilter rdr (Just emptyUserDataFilter)

  tags <- TMR.read rdr 1000
  putStrLn $ "read " ++ show (length tags) ++ " tags"
  mapM_ T.putStrLn $ concatMap TMR.displayTagReadData tags

  TMR.destroy rdr
