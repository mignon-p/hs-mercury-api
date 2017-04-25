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

  putStrLn "setting parameters"
  plan <- TMR.paramGet rdr TMR.PARAM_READ_PLAN
  putStrLn "setting region"
  TMR.paramSet rdr TMR.PARAM_REGION_ID TMR.REGION_NA2
  putStrLn "setting read power"
  TMR.paramSet rdr TMR.PARAM_RADIO_READPOWER (500 :: Int32)
  let plan' = plan { TMR.rpAntennas = [1] }
  putStrLn "setting plan"
  TMR.paramSet rdr TMR.PARAM_READ_PLAN plan'

  putStrLn "reading tags"
  forM_ [1..10] $ \_ -> do
    tags <- TMR.read rdr 500
    putStrLn "here are the tags:"
    mapM_ T.putStrLn $ concatMap TMR.displayTagReadData tags
    putStrLn ""

  TMR.destroy rdr
