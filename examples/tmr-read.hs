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
import System.IO

main = do
  rdr <- TMR.create "tmr:///dev/ttyUSB0"
  listener <- TMR.hexListener stdout
  TMR.addTransportListener rdr listener
  TMR.paramSet rdr TMR.PARAM_TRANSPORTTIMEOUT (10000 :: Word32)
  TMR.connect rdr

  TMR.paramSet rdr TMR.PARAM_REGION_ID TMR.REGION_NA2
  TMR.paramSet rdr TMR.PARAM_RADIO_READPOWER (500 :: Int32)
  let tagop = TMR.TagOp_GEN2_ReadData
              { TMR.opBank = TMR.GEN2_BANK_TID
              , TMR.opExtraBanks = [TMR.GEN2_BANK_EPC, TMR.GEN2_BANK_TID, TMR.GEN2_BANK_USER]
              , TMR.opWordAddress = 0
              , TMR.opLen = 32
              }
      plan = TMR.antennaReadPlan { TMR.rpTagop = Just tagop }
  TMR.paramSet rdr TMR.PARAM_READ_PLAN plan

  tags <- TMR.read rdr 1000
  putStrLn $ "read " ++ show (length tags) ++ " tags"
  mapM_ T.putStrLn $ concatMap TMR.displayTagReadData tags

  TMR.destroy rdr
