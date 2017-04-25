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
import System.Environment
import System.Exit
import System.IO

printFwVers :: TMR.Reader -> IO ()
printFwVers rdr =
  TMR.paramGet rdr TMR.PARAM_VERSION_SOFTWARE >>= T.putStrLn

main = do
  args <- getArgs
  let argsLen = length args
  when (argsLen < 1 || argsLen > 2) $ do
    putStrLn "Usage: tmr-firmware URI [firmware-file]"
    putStrLn "Example: tmr-firmware tmr:///dev/ttyUSB0 NanoFW-1.7.1.2.sim"
    putStrLn "Firmware file can be downloaded from:"
    putStrLn "http://www.thingmagic.com/index.php/download-nano-firmware"
    putStrLn "If run without firmware file, just print current firmware version."
    exitFailure
  let uri = args !! 0

  rdr <- TMR.create (T.pack uri)
  putStrLn $ "connecting to " ++ uri
  TMR.connect rdr

  putStr "Current firmware version: "
  printFwVers rdr

  when (argsLen < 2) exitSuccess
  let fwFile = args !! 1

  putStrLn $ "Loading firmware image from " ++ fwFile
  TMR.firmwareLoadFile rdr fwFile

  putStr "New firmware version: "
  printFwVers rdr

  TMR.destroy rdr
