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
import System.Console.ANSI
import qualified System.Hardware.MercuryApi as TMR
import qualified System.Hardware.MercuryApi.Params as TMR
import System.Exit
import System.IO
import Text.Printf

data Opts = Opts
  { oUri :: String
  , oRegion :: String
  , oPower :: Int32
  , oListen :: Bool
  , oLong :: Bool
  }

opts :: Parser Opts
opts = Opts
  <$> strOption (long "uri" <>
                 short 'u' <>
                 metavar "URI" <>
                 help ("Reader to connect to (default " ++ defUri ++ ")") <>
                 value defUri)
  <*> strOption (long "region" <>
                 short 'r' <>
                 metavar "REGION" <>
                 help ("Regulatory region (default " ++ defRegion ++ ")") <>
                 value defRegion)
  <*> option auto (long "power" <>
                   short 'p' <>
                   metavar "CENTI-DBM" <>
                   help ("Power level (0-2700, default " ++ show defPower ++ ")") <>
                   value defPower)
  <*> switch (long "transport-listener" <>
              short 't' <>
              help "Print bytes sent on serial port")
  <*> switch (long "long" <>
              short 'l' <>
              help "Print lots of information per tag")
  where
    defUri = "tmr:///dev/ttyUSB0"
    defRegion = "na2"
    defPower = 2300

opts' = info (helper <*> opts)
  ( fullDesc <>
    header "tmr-read - read tags" )

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

displayTag :: TMR.TagReadData -> T.Text
displayTag trd = strength <> " <" <> epc <> "> " <> user
  where
    strength = T.pack $ printf "%3d" (TMR.trRssi trd)
    epc = TMR.bytesToHex $ TMR.tdEpc $ TMR.trTag trd
    user = T.pack $ show $ B.takeWhile (/= 0) (TMR.trData trd)

main = do
  o <- execParser opts'

  rdr <- TMR.create $ T.pack $ oUri o
  when (oListen o) $ do
    listener <- TMR.hexListener stdout
    void $ TMR.addTransportListener rdr listener
  TMR.paramSet rdr TMR.PARAM_TRANSPORTTIMEOUT (10000 :: Word32)
  TMR.connect rdr

  rgn <- parseRegionOrFail rdr (oRegion o)
  eth <- try $ TMR.paramSetBasics rdr rgn (oPower o) TMR.sparkFunAntennas
  handleParamError rdr eth
  TMR.paramSetTagReadDataRecordHighestRssi rdr True
  TMR.paramSetReadPlanTagop rdr (Just readUser)

  tags <- TMR.read rdr 1000
  let tags' = reverse $ sortBy (comparing TMR.trRssi) tags
  putStrLn $ "read " ++ show (length tags') ++ " tags"
  if oLong o
    then zipWithM_ printInColor (map TMR.displayTagReadData tags') [0..]
    else forM_ tags' (T.putStrLn . displayTag)

  TMR.destroy rdr
