{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Int
import Data.List
import Data.Monoid
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word
import qualified System.Hardware.MercuryApi as TMR
import qualified System.Hardware.MercuryApi.Params as TMR
import System.Environment
import System.IO

delayMillis :: Int
delayMillis = 20

mkPin :: TMR.PinNumber -> TMR.PinNumber -> TMR.GpioPin
mkPin highPin pin =
  TMR.GpioPin
  { TMR.gpId = pin
  , TMR.gpHigh = highPin == pin
  , TMR.gpOutput = True
  }

gpioLoop :: TMR.Reader -> [TMR.PinNumber] -> Integer -> [TMR.GpioPin] -> IO ()
gpioLoop rdr outPins millis oldPins = do
  pins <- TMR.gpiGet rdr
  when (pins /= oldPins) $ print pins
  let oLen = length outPins
      pinNo = fromIntegral $ (millis `div` 1000) `mod` fromIntegral oLen
      gpoPins = map (mkPin pinNo) outPins
  TMR.gpoSet rdr gpoPins
  threadDelay $ delayMillis * 1000
  gpioLoop rdr outPins (millis + fromIntegral delayMillis) pins

main = do
  rdr <- TMR.create "tmr:///dev/ttyUSB0"
  listener <- TMR.hexListener stdout
  TMR.addTransportListener rdr listener
  TMR.paramSetTransportTimeout rdr 10000
  TMR.connect rdr

  args <- getArgs
  let outPins = map read args
  TMR.paramSetGpioOutputList rdr outPins

  gpioLoop rdr outPins 0 [] `finally` TMR.destroy rdr
