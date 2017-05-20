{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Int
import Data.List
import Data.Monoid
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as T
import Data.Word
import Options.Applicative
import qualified System.Hardware.MercuryApi as TMR
import qualified System.Hardware.MercuryApi.Params as TMR
import System.IO

import ExampleUtil

data Opts = Opts
  { oUri :: String
  , oRegion :: String
  , oPower :: Int32
  , oListen :: Bool
  }

opts :: Parser Opts
opts = Opts
  <$> optUri
  <*> optRegion
  <*> optPower
  <*> optListen

opts' = info (helper <*> opts)
  ( fullDesc <>
    header "tmr-lock - test that locking works" )

password :: TMR.GEN2_Password
password = 12345

main = do
  o <- execParser opts'

  rdr <- createConnectAndParams (oUri o) (oListen o) (oRegion o) (oPower o)

  tags <- TMR.read rdr 1000
  putStrLn $ "read " ++ show (length tags) ++ " tags"
  when (not $ null tags) $ do
    let trd = maximumBy (comparing TMR.trRssi) tags
        td = TMR.trTag trd
        epc = TMR.tdEpc td
        hex = TMR.bytesToHex epc

    T.putStrLn $ "writing password to <" <> hex <> ">"
    let epcFilt = TMR.TagFilterEPC td
        opWrite = TMR.TagOp_GEN2_WriteData
                  { TMR.opBank = TMR.GEN2_BANK_RESERVED
                  , TMR.opWordAddress = TMR.accessPasswordAddress
                  , TMR.opData = TMR.passwordToWords password
                  }
    TMR.executeTagOp rdr opWrite (Just epcFilt)

    T.putStrLn $ "locking <" <> hex <> ">"
    let opLock = TMR.TagOp_GEN2_Lock
                 { TMR.opMask   = [TMR.GEN2_LOCK_BITS_USER]
                 , TMR.opAction = [TMR.GEN2_LOCK_BITS_USER]
                 , TMR.opAccessPassword = password
                 }
    TMR.executeTagOp rdr opLock (Just epcFilt)

    T.putStrLn $ "attempting to write user data to <" <> hex <> ">"
    let opWrite2 = TMR.TagOp_GEN2_WriteData
                  { TMR.opBank = TMR.GEN2_BANK_USER
                  , TMR.opWordAddress = 0
                  , TMR.opData = TMR.packBytesIntoWords "This should fail"
                  }
    eth <- try $ TMR.executeTagOp rdr opWrite2 (Just epcFilt)
    case eth of
      Right _ ->
        T.putStrLn "Write succeeded, but it shouldn't have."
      Left err ->
        when (TMR.meStatus err /= TMR.ERROR_PROTOCOL_WRITE_FAILED &&
              TMR.meStatus err /= TMR.ERROR_PROTOCOL_BIT_DECODING_FAILED) $ throw err

    T.putStrLn $ "unlocking <" <> hex <> ">"
    let opUnlock = TMR.TagOp_GEN2_Lock
                   { TMR.opMask   = [TMR.GEN2_LOCK_BITS_USER]
                   , TMR.opAction = []
                   , TMR.opAccessPassword = password
                   }
    void $ TMR.executeTagOp rdr opUnlock (Just epcFilt)

  TMR.destroy rdr
