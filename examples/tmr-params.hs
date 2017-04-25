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

stringParams :: [TMR.Param]
stringParams =
  [ TMR.PARAM_VERSION_HARDWARE
  , TMR.PARAM_VERSION_SERIAL
  , TMR.PARAM_VERSION_MODEL
  , TMR.PARAM_VERSION_SOFTWARE
  , TMR.PARAM_URI
  , TMR.PARAM_PRODUCT_GROUP
  {-
  , TMR.PARAM_READER_DESCRIPTION
  , TMR.PARAM_READER_HOSTNAME
  -}
  ]

main = do
  T.putStrLn $ "API version: " <> TMR.apiVersion
  putStrLn "create"
  rdr <- TMR.create "tmr:///dev/ttyUSB0"
  putStrLn "addTransportListener"
  TMR.addTransportListener rdr listener
  putStrLn "paramGet PARAM_TRANSPORTTIMEOUT"
  timeout <- TMR.paramGet rdr TMR.PARAM_TRANSPORTTIMEOUT :: IO Word32
  print timeout
  putStrLn "connect"
  TMR.connect rdr
  putStrLn "paramList"
  params <- TMR.paramList rdr
  forM_ params $ \param -> do
    putStrLn $ show param ++ " - " ++ T.unpack (TMR.paramName param)
  forM_ stringParams $ \param -> do
    putStrLn $ "paramGet " ++ show param
    txt <- TMR.paramGet rdr param
    T.putStrLn txt
  putStrLn "paramGet PARAM_REGION_SUPPORTEDREGIONS"
  regions <- TMR.paramGet rdr TMR.PARAM_REGION_SUPPORTEDREGIONS :: IO [TMR.Region]
  print regions
  putStrLn "paramGet PARAM_VERSION_SUPPORTEDPROTOCOLS"
  protos <- TMR.paramGet rdr TMR.PARAM_VERSION_SUPPORTEDPROTOCOLS :: IO [TMR.TagProtocol]
  print protos
  putStrLn "paramGet PARAM_READ_PLAN"
  plan <- TMR.paramGet rdr TMR.PARAM_READ_PLAN :: IO TMR.ReadPlan
  print plan
  putStrLn "paramGet PARAM_REGION_ID"
  region <- TMR.paramGet rdr TMR.PARAM_REGION_ID :: IO TMR.Region
  print region
  putStrLn "paramGet PARAM_METADATAFLAG"
  meta <- TMR.paramGet rdr TMR.PARAM_METADATAFLAG :: IO [TMR.MetadataFlag]
  print meta

  putStrLn ""
  putStrLn "paramSet PARAM_REGION_ID"
  TMR.paramSet rdr TMR.PARAM_REGION_ID TMR.REGION_NA2

  putStrLn "paramSet PARAM_RADIO_READPOWER"
  TMR.paramSet rdr TMR.PARAM_RADIO_READPOWER (500 :: Int32)

  putStrLn "paramSet PARAM_READ_PLAN"
  let plan' = plan { rpAntennas = [1] }
  TMR.paramSet rdr TMR.PARAM_READ_PLAN plan'

  putStrLn ""
  putStrLn "read"
  tags <- TMR.read rdr 500
  print tags

  putStrLn ""
  putStrLn "destroy"
  TMR.destroy rdr
