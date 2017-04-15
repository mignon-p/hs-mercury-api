{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Text as T
import qualified System.Hardware.MercuryApi as TMR

repeatedConnect :: TMR.Reader -> Int -> IO ()
repeatedConnect rdr n
  | n <= 1 = TMR.connect rdr
  | otherwise = do
      eth <- try (TMR.connect rdr)
      case eth of
        Left exc -> do
          print (TMR.meStatus exc)
	  threadDelay 100000
          repeatedConnect rdr (n - 1)
        _ -> return ()

main = do
  putStrLn "create"
  rdr <- TMR.create "tmr:///dev/ttyUSB0"
  putStrLn "connect"
  repeatedConnect rdr 100
  putStrLn "paramList"
  params <- TMR.paramList rdr
  putStrLn "destroy"
  TMR.destroy rdr
  forM_ params $ \param -> do
    putStrLn $ show param ++ " - " ++ T.unpack (TMR.paramName param)
