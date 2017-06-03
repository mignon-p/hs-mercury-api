{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent ( threadDelay )
import Control.Exception ( finally )
import Control.Monad ( when )
import Data.Monoid ( (<>) )
import qualified Data.Text.IO as T ( putStrLn )
import Options.Applicative
    ( Alternative(many),
      Applicative((<*>)),
      Parser,
      helper,
      execParser,
      metavar,
      info,
      header,
      fullDesc,
      auto,
      argument,
      (<$>) )
import qualified System.Hardware.MercuryApi as TMR
import qualified System.Hardware.MercuryApi.Params as TMR

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
  rdr <- createAndConnect (oUri o) (oListen o)

  -- set all the pins to input
  TMR.paramSetGpioInputList rdr [1..4]

  -- then set some to output
  let outPins = oGpos o
  TMR.paramSetGpioOutputList rdr outPins
  putStrLn $ "cycling among output pins " ++ show outPins

  -- now see which inputs are left
  inPins <- TMR.paramGetGpioInputList rdr
  putStrLn $ "listening on input pins " ++ show inPins

  gpioLoop rdr outPins 0 [] `finally` TMR.destroy rdr
