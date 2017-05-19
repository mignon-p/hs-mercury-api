{-# LANGUAGE OverloadedStrings #-}

module ExampleUtil
  ( createAndConnect
  , createConnectAndParams
  , defUri
  , defRegion
  , defPower
  ) where

import Control.Exception
import Control.Monad
import Data.Int
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word
import qualified System.Hardware.MercuryApi as TMR
import qualified System.Hardware.MercuryApi.Params as TMR
import System.Exit
import System.IO

printRegionsAndFail :: TMR.Reader -> IO a
printRegionsAndFail rdr = do
  rgns <- TMR.paramGetRegionSupportedRegions rdr
  T.putStrLn "Region must be one of:"
  forM_ rgns $ \rgn -> T.putStrLn $ "  " <> TMR.displayRegion rgn
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
  TMR.paramSet rdr TMR.PARAM_TRANSPORTTIMEOUT (10000 :: Word32)
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
defUri = "tmr:///dev/ttyUSB0"

defRegion :: String
defRegion = "na2"

defPower :: Int32
defPower = 2300
