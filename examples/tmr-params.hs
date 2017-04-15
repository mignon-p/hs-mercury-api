{-# LANGUAGE OverloadedStrings #-}

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
        Left exc ->
          case TMR.meStatus exc of
            TMR.ERROR_TIMEOUT -> do
              print (TMR.meStatus exc)
              repeatedConnect rdr (n - 1)
            TMR.ERROR_DEVICE_RESET -> do
              print (TMR.meStatus exc)
              repeatedConnect rdr (n - 1)
            _ -> throwIO exc
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
