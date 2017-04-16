{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified System.Hardware.MercuryApi as TMR
import Text.Printf

listener :: TMR.TransportListener
listener tx dat _ = lstn dat (prefix tx)
  where
    prefix True  = "Sending: "
    prefix False = "Received:"
    lstn bs pfx = do
      let (bs1, bs2) = B.splitAt 16 bs
          hex = concatMap (printf " %02x") (B.unpack bs1)
      putStrLn $ pfx ++ hex
      when (not $ B.null bs2) $ lstn bs2 "         "

main = do
  putStrLn "create"
  rdr <- TMR.create "tmr:///dev/ttyUSB0"
  putStrLn "addTransportListener"
  TMR.addTransportListener rdr listener
  putStrLn "connect"
  TMR.connect rdr
  putStrLn "paramList"
  params <- TMR.paramList rdr
  putStrLn "destroy"
  TMR.destroy rdr
  forM_ params $ \param -> do
    putStrLn $ show param ++ " - " ++ T.unpack (TMR.paramName param)
