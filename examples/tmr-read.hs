{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ( when, forM_ )
import qualified Data.ByteString as B ( takeWhile, null )
import Data.Int ( Int32 )
import Data.List ( sortBy )
import Data.Monoid ( (<>) )
import Data.Ord ( comparing )
import qualified Data.Text as T ( Text, takeWhile, pack, null )
import qualified Data.Text.IO as T ( putStrLn )
import Options.Applicative
    ( Applicative((<*>)),
      Parser,
      helper,
      execParser,
      switch,
      short,
      long,
      info,
      help,
      header,
      fullDesc,
      (<$>) )
import System.Console.ANSI
    ( SGR(Reset, SetColor, SetConsoleIntensity),
      Color(Blue),
      ColorIntensity(Vivid),
      ConsoleIntensity(BoldIntensity),
      ConsoleLayer(Foreground),
      setSGR )
import qualified System.Hardware.MercuryApi as TMR
import Text.Printf ( printf )

import ExampleUtil

data Opts = Opts
  { oUri :: String
  , oRegion :: String
  , oPower :: Int32
  , oListen :: Bool
  , oLong :: Bool
  }

opts :: Parser Opts
opts = Opts
  <$> optUri
  <*> optRegion
  <*> optPower
  <*> optListen
  <*> switch (long "long" <>
              short 'l' <>
              help "Print lots of information per tag")

opts' = info (helper <*> opts)
  ( fullDesc <>
    header "tmr-read - read tags" )

readUser =
  TMR.TagOp_GEN2_ReadData
  { TMR.opBank = TMR.GEN2_BANK_USER
  , TMR.opExtraBanks = []
  , TMR.opWordAddress = 0
  , TMR.opLen = 32
  }

putWithBold :: T.Text -> IO ()
putWithBold txt = do
  let bold = T.null $ T.takeWhile (== ' ') txt
  when bold $ setSGR [SetConsoleIntensity BoldIntensity]
  T.putStrLn txt
  when bold $ setSGR [Reset]

displayTag :: TMR.TagReadData -> T.Text
displayTag trd = strength <> " <" <> epc <> ">" <> user
  where
    strength = T.pack $ printf "%3d" (TMR.trRssi trd)
    epc = TMR.bytesToHex $ TMR.tdEpc $ TMR.trTag trd
    dat = TMR.trData trd
    user =
      if B.null dat
      then "" -- this means it failed to read the user data
      else T.pack $ ' ' : show (B.takeWhile (/= 0) dat)

main = do
  o <- execParser opts'

  rdr <- createConnectAndParams (oUri o) (oListen o) (oRegion o) (oPower o)
  TMR.paramSetReadPlanTagop rdr (Just readUser)

  tags <- TMR.read rdr 1000
  let tags' = reverse $ sortBy (comparing TMR.trRssi) tags
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn $ "read " ++ show (length tags') ++ " tags"
  setSGR [Reset]
  if oLong o
    then mapM_ putWithBold (concatMap TMR.displayTagReadData tags')
    else forM_ tags' (T.putStrLn . displayTag)

  TMR.destroy rdr
