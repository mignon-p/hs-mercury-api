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
import System.Console.ANSI
import qualified System.Hardware.MercuryApi as TMR
import qualified System.Hardware.MercuryApi.Params as TMR
import System.IO

import ExampleUtil

data Opts = Opts
  { oUri :: String
  , oRegion :: String
  , oPower :: Int32
  , oListen :: Bool
  , oRewrite :: Bool
  , oString :: String
  }

opts :: Parser Opts
opts = Opts
  <$> optUri
  <*> optRegion
  <*> optPower
  <*> optListen
  <*> switch (long "rewrite" <>
              short 'R' <>
              help "Write to a tag even if written before")
  <*> argument str (metavar "STRING")

opts' = info (helper <*> opts)
  ( fullDesc <>
    header "tmr-write - write a string to user data of tag" )

emptyUserDataFilter :: TMR.TagFilter
emptyUserDataFilter = TMR.mkFilterGen2 TMR.GEN2_BANK_USER 0 $ B.pack [0, 0]

printColor :: Color -> T.Text -> IO ()
printColor c txt = do
  setSGR [SetColor Foreground Vivid c]
  T.putStrLn txt
  setSGR [Reset]

main = do
  o <- execParser opts'

  rdr <- createConnectAndParams (oUri o) (oListen o) (oRegion o) (oPower o)
  when (not $ oRewrite o) $
    TMR.paramSetReadPlanFilter rdr (Just emptyUserDataFilter)

  tags <- TMR.read rdr 1000
  putStrLn $ "read " ++ show (length tags) ++ " tags"
  if null tags
    then do
    printColor Red "No tag found"
    else do
    let trd = maximumBy (comparing TMR.trRssi) tags
        td = TMR.trTag trd
        epc = TMR.tdEpc td
        hex = TMR.bytesToHex epc
    T.putStrLn $ "writing <" <> hex <> ">"
    let epcFilt = TMR.TagFilterEPC td
        txt = T.pack (oString o)
        words = TMR.packBytesIntoWords $ T.encodeUtf8 txt `B.snoc` 0
        opWrite = TMR.TagOp_GEN2_WriteData
                  { TMR.opBank = TMR.GEN2_BANK_USER
                  , TMR.opWordAddress = 0
                  , TMR.opData = words
                  }
    TMR.executeTagOp rdr opWrite (Just epcFilt)
    printColor Green "Success!"

  TMR.destroy rdr
