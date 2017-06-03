{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ( when )
import qualified Data.ByteString as B ( snoc, pack )
import Data.Int ( Int32 )
import Data.List ( maximumBy )
import Data.Monoid ( (<>) )
import Data.Ord ( comparing )
import qualified Data.Text as T ( Text, pack )
import qualified Data.Text.Encoding as T ( encodeUtf8 )
import qualified Data.Text.IO as T ( putStrLn )
import Options.Applicative
    ( Applicative((<*>)),
      Parser,
      helper,
      execParser,
      switch,
      str,
      short,
      metavar,
      long,
      info,
      help,
      header,
      fullDesc,
      argument,
      (<$>) )
import System.Console.ANSI
    ( SGR(Reset, SetColor),
      Color(Green, Red),
      ColorIntensity(Vivid),
      ConsoleLayer(Foreground),
      setSGR )
import qualified System.Hardware.MercuryApi as TMR

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
