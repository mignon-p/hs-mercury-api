{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ( when )
import Data.Monoid ( (<>) )
import qualified Data.Text.IO as T ( putStrLn )
import Options.Applicative
    ( Applicative((<*>)),
      Parser,
      helper,
      execParser,
      str,
      progDesc,
      metavar,
      info,
      header,
      fullDesc,
      argument,
      (<$>),
      optional )
import qualified System.Hardware.MercuryApi as TMR
import System.Exit ( exitSuccess )

import ExampleUtil

data Opts = Opts
  { oUri :: String
  , oListen :: Bool
  , oFilename :: Maybe String
  }

opts :: Parser Opts
opts = Opts
  <$> optUri
  <*> optListen
  <*> optional (argument str (metavar "FILENAME"))

opts' = info (helper <*> opts)
  ( fullDesc <>
    progDesc ("If run without firmware file, just print current " ++
              "firmware version.  " ++
              "Firmware file can be downloaded from: " ++
              "http://www.thingmagic.com/index.php/download-nano-firmware") <>
    header "tmr-firmware - write a new firmware image to reader" )

printFwVers :: TMR.Reader -> IO ()
printFwVers rdr =
  TMR.paramGet rdr TMR.PARAM_VERSION_SOFTWARE >>= T.putStrLn

main = do
  o <- execParser opts'
  rdr <- createAndConnect (oUri o) (oListen o)

  putStr "Current firmware version: "
  printFwVers rdr

  when (oFilename o == Nothing) exitSuccess
  let (Just fwFile) = oFilename o

  putStrLn $ "Loading firmware image from " ++ fwFile
  TMR.firmwareLoadFile rdr fwFile

  putStr "New firmware version: "
  printFwVers rdr

  TMR.destroy rdr
