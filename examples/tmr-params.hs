{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.Text as T
import qualified System.Hardware.MercuryApi as TMR

main = do
  rdr <- TMR.create "tmr:///dev/ttyUSB0"
  TMR.connect rdr
  params <- TMR.paramList rdr
  TMR.destroy rdr
  forM_ params $ \param -> do
    putStrLn $ show param ++ " - " ++ T.unpack (TMR.paramName param)
