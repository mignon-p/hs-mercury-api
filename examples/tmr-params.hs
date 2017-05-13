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
import System.Console.ANSI
import qualified System.Hardware.MercuryApi as TMR
import qualified System.Hardware.MercuryApi.Params as TMR
import System.IO

main = do
  T.putStrLn $ "API version: " <> TMR.apiVersion
  rdr <- TMR.create "tmr:///dev/ttyUSB0"
  {-
  listener <- TMR.hexListener stdout
  TMR.addTransportListener rdr listener
  -}
  TMR.paramSetTransportTimeout rdr 10000
  TMR.connect rdr
  params <- TMR.paramList rdr
  forM_ params $ \param -> do
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn $ show param
    setSGR [Reset]
    T.putStrLn $ " - " <> TMR.paramName param
    let typ = TMR.displayParamType $ TMR.paramType param
    eth <- try $ TMR.paramGetString rdr param
    case eth of
      Left err -> do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ "  " ++ show (TMR.meStatus err)
        setSGR [Reset]
      Right txt -> do
        setSGR [SetColor Foreground Dull Green]
        T.putStr $ "  " <> txt
        setSGR [Reset]
        T.putStrLn $ " :: " <> typ
  TMR.destroy rdr
