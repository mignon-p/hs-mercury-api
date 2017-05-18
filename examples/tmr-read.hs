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
import Options.Applicative
import System.Console.ANSI
import qualified System.Hardware.MercuryApi as TMR
import System.IO

data Opts = Opts
  { oUri :: String
  , oPower :: Int32
  , oListen :: Bool
  }

opts :: Parser Opts
opts = Opts
  <$> strOption (long "uri" <>
                 short 'u' <>
                 metavar "URI" <>
                 help ("Reader to connect to (default " ++ defUri ++ ")") <>
                 value defUri)
  <*> option auto (long "power" <>
                   short 'p' <>
                   metavar "CENTI-DBM" <>
                   help ("Power level (0-2700, default " ++ show defPower ++ ")") <>
                   value defPower)
  <*> switch (long "transport-listener" <>
              short 't' <>
              help "Print bytes sent on serial port")
  where
    defUri = "tmr:///dev/ttyUSB0"
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

main = do
  o <- execParser opts'

  rdr <- TMR.create $ T.pack $ oUri o
  when (oListen o) $ do
    listener <- TMR.hexListener stdout
    void $ TMR.addTransportListener rdr listener
  TMR.paramSet rdr TMR.PARAM_TRANSPORTTIMEOUT (10000 :: Word32)
  TMR.connect rdr

  TMR.paramSetBasics rdr TMR.REGION_NA2 (oPower o) TMR.sparkFunAntennas
  TMR.paramSetReadPlanTagop rdr (Just readUser)

  tags <- TMR.read rdr 1000
  putStrLn $ "read " ++ show (length tags) ++ " tags"
  zipWithM_ printInColor (map TMR.displayTagReadData tags) [0..]

  TMR.destroy rdr
