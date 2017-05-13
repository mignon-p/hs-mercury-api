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
  timeout <- TMR.paramGet rdr TMR.PARAM_TRANSPORTTIMEOUT :: IO Word32
  print timeout
  TMR.connect rdr
  params <- TMR.paramList rdr
  forM_ params $ \param -> do
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn $ show param ++ " - " ++ T.unpack (TMR.paramName param)
    let typ = TMR.displayParamType $ TMR.paramType param
    eth <- try $ TMR.paramGetString rdr param
    case eth of
      Left err -> do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ "  " ++ show (TMR.meStatus err)
      Right txt -> do
        setSGR [SetColor Foreground Vivid Green]
        T.putStrLn $ "  " <> txt <> " :: " <> typ
    setSGR [Reset]
  TMR.destroy rdr
