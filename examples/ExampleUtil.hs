{-# LANGUAGE OverloadedStrings #-}

module ExampleUtil
  ( createAndConnect
  , createConnectAndParams
  , optUri
  , optRegion
  , optPower
  , optListen
  ) where

import Control.Exception
import Control.Monad
import Data.Int
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word
import Options.Applicative
import qualified System.Hardware.MercuryApi as TMR
import qualified System.Hardware.MercuryApi.Params as TMR
import System.Exit
import System.Info
import System.IO

printRegionsAndFail :: TMR.Reader -> IO a
printRegionsAndFail rdr = do
  rgns <- TMR.paramGetRegionSupportedRegions rdr
  T.putStrLn "Region must be one of:"
  forM_ rgns $ \rgn -> do
    let dr = TMR.displayRegion rgn
        nSpaces = 6 - T.length dr
        spaces = T.replicate nSpaces " "
    T.putStrLn $ "  " <> dr <> spaces <> TMR.displayRegionDescription rgn
  exitFailure

parseRegionOrFail :: TMR.Reader -> String -> IO TMR.Region
parseRegionOrFail rdr s =
  case TMR.parseRegion (T.pack s) of
    Nothing -> printRegionsAndFail rdr
    Just rgn -> return rgn

printPowerAndFail :: TMR.Reader -> IO a
printPowerAndFail rdr = do
  lo <- TMR.paramGetRadioPowerMin rdr
  hi <- TMR.paramGetRadioPowerMax rdr
  putStrLn $ "Power must be between " ++ show lo ++ " and " ++ show hi
  exitFailure

handleParamError :: TMR.Reader -> Either TMR.MercuryException () -> IO ()
handleParamError _ (Right _) = return ()
handleParamError rdr (Left err) = hpe (TMR.meStatus err)
  where hpe TMR.ERROR_INVALID_REGION = printRegionsAndFail rdr
        hpe TMR.ERROR_MSG_POWER_TOO_HIGH = printPowerAndFail rdr
        hpe TMR.ERROR_MSG_POWER_TOO_LOW = printPowerAndFail rdr
        hpe _ = throw err

createAndConnect :: String -> Bool -> IO TMR.Reader
createAndConnect uri listen = do
  rdr <- TMR.create $ T.pack uri
  when (listen) $ do
    listener <- TMR.hexListener stdout
    void $ TMR.addTransportListener rdr listener
  TMR.paramSetTransportTimeout rdr 10000
  TMR.connect rdr
  return rdr

createConnectAndParams :: String -> Bool -> String -> Int32 -> IO TMR.Reader
createConnectAndParams uri listen region power = do
  rdr <- createAndConnect uri listen
  rgn <- parseRegionOrFail rdr region
  eth <- try $ TMR.paramSetBasics rdr rgn power TMR.sparkFunAntennas
  handleParamError rdr eth
  TMR.paramSetTagReadDataRecordHighestRssi rdr True
  return rdr

defUri :: String
defUri = case os of
           "darwin" -> "tmr:///dev/cu.SLAB_USBtoUART"
           _ -> "tmr:///dev/ttyUSB0"

defRegion :: String
defRegion = "na2"

defPower :: Int32
defPower = 2300

optUri :: Parser String
optUri = strOption (long "uri" <>
                    short 'u' <>
                    metavar "URI" <>
                    help ("Reader to connect to (default " ++ defUri ++ ")") <>
                    value defUri)

optRegion :: Parser String
optRegion = strOption (long "region" <>
                       short 'r' <>
                       metavar "REGION" <>
                       help ("Regulatory region (default " ++ defRegion ++ ")") <>
                       value defRegion)

optPower :: Parser Int32
optPower = option auto (long "power" <>
                        short 'p' <>
                        metavar "CENTI-DBM" <>
                        help ("Power level (0-2700, default " ++ show defPower ++ ")") <>
                        value defPower)

optListen :: Parser Bool
optListen = switch (long "transport-listener" <>
                    short 't' <>
                    help "Print bytes sent on serial port")
