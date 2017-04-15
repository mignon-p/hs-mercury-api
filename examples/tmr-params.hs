{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.Text as T
import qualified System.Hardware.MercuryApi as TMR

main = do
  putStrLn "create"
  rdr <- TMR.create "tmr:///dev/ttyUSB0"
  putStrLn "connect"
  TMR.connect rdr
  putStrLn "paramList"
  params <- TMR.paramList rdr
  putStrLn "destroy"
  TMR.destroy rdr
  forM_ params $ \param -> do
    putStrLn $ show param ++ " - " ++ T.unpack (TMR.paramName param)
