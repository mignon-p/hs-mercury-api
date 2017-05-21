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
import Options.Applicative
import qualified System.Hardware.MercuryApi as TMR
import qualified System.Hardware.MercuryApi.Params as TMR
import System.IO

import ExampleUtil

data Opts = Opts
  { oUri :: String
  , oListen :: Bool
  , oGpos :: [TMR.PinNumber]
  }

opts :: Parser Opts
opts = Opts
  <$> optUri
  <*> optListen
  <*> many (argument auto (metavar "OUTPUTS..."))

opts' = info (helper <*> opts)
  ( fullDesc <>
    header "tmr-gpio - print GPIs and control GPOs" )

delayMillis :: Int
delayMillis = 100

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
  when (pins /= oldPins) $ mapM_ T.putStrLn $ TMR.displayGpio pins
  when (not $ null outPins) $ do
    let oLen = length outPins
        pinNo = outPins !! fromIntegral ((millis `div` 1000) `mod` fromIntegral oLen)
        gpoPins = map (mkPin pinNo) outPins
    TMR.gpoSet rdr gpoPins
  threadDelay $ delayMillis * 1000
  gpioLoop rdr outPins (millis + fromIntegral delayMillis) pins

main = do
  o <- execParser opts'
  rdr1 <- createAndConnect (oUri o) (oListen o)

  -- reboot reader to reset the list of input and output pins
  TMR.reboot rdr1
  TMR.destroy rdr1

  rdr <- createAndConnect (oUri o) (oListen o)

  let outPins = oGpos o
  TMR.paramSetGpioOutputList rdr outPins
  putStrLn $ "cycling among output pins " ++ show outPins

  inPins <- TMR.paramGetGpioInputList rdr
  putStrLn $ "listening on input pins " ++ show inPins

  gpioLoop rdr outPins 0 [] `finally` TMR.destroy rdr
