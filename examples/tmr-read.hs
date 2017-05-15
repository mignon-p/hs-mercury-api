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
import System.Console.ANSI
import qualified System.Hardware.MercuryApi as TMR
import System.IO

readAll =
  TMR.TagOp_GEN2_ReadData
  { TMR.opBank = TMR.GEN2_BANK_TID
  , TMR.opExtraBanks = [TMR.GEN2_BANK_EPC, TMR.GEN2_BANK_TID, TMR.GEN2_BANK_USER]
  , TMR.opWordAddress = 0
  , TMR.opLen = 32
  }

readUser =
  TMR.TagOp_GEN2_ReadData
  { TMR.opBank = TMR.GEN2_BANK_USER
  , TMR.opExtraBanks = []
  , TMR.opWordAddress = 0
  , TMR.opLen = 32
  }

printInColor :: [T.Text] -> Int -> IO ()
printInColor xs n = do
  let color = if n `mod` 2 == 0 then Blue else Green
  setSGR [SetColor Foreground Vivid color]
  mapM_ T.putStrLn xs
  setSGR [Reset]

main = do
  rdr <- TMR.create "tmr:///dev/ttyUSB0"
  listener <- TMR.hexListener stdout
  TMR.addTransportListener rdr listener
  TMR.paramSet rdr TMR.PARAM_TRANSPORTTIMEOUT (10000 :: Word32)
  TMR.connect rdr

  TMR.paramSet rdr TMR.PARAM_REGION_ID TMR.REGION_NA2
  TMR.paramSet rdr TMR.PARAM_RADIO_READPOWER (500 :: Int32)
  let plan = TMR.defaultReadPlan { TMR.rpTagop = Just readUser
                                 , TMR.rpAntennas = TMR.sparkFunAntennas
                                 }
  TMR.paramSet rdr TMR.PARAM_READ_PLAN plan

  tags <- TMR.read rdr 1000
  putStrLn $ "read " ++ show (length tags) ++ " tags"
  zipWithM_ printInColor (map TMR.displayTagReadData tags) [0..]

  TMR.destroy rdr
