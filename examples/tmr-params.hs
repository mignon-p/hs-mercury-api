{-# LANGUAGE OverloadedStrings #-}

import Control.Exception ( try )
import Control.Monad ( forM_ )
import Data.List ( sortBy )
import Data.Monoid ( (<>) )
import qualified Data.Text as T ( dropWhileEnd )
import qualified Data.Text.IO as T ( putStrLn, putStr )
import Options.Applicative
    ( Applicative((<*>)),
      Parser,
      helper,
      execParser,
      info,
      header,
      fullDesc,
      (<$>) )
import System.Console.ANSI
    ( SGR(Reset, SetColor),
      Color(Blue, Green, Red),
      ColorIntensity(Dull, Vivid),
      ConsoleLayer(Foreground),
      setSGR )
import qualified System.Hardware.MercuryApi as TMR
import qualified System.Hardware.MercuryApi.Params as TMR

import ExampleUtil

data Opts = Opts
  { oUri :: String
  , oListen :: Bool
  }

opts :: Parser Opts
opts = Opts
  <$> optUri
  <*> optListen

opts' = info (helper <*> opts)
  ( fullDesc <>
    header "tmr-params - print parameters" )

compareParams :: TMR.Param -> TMR.Param -> Ordering
compareParams p1 p2 =
  let n1 = TMR.paramName p1
      n2 = TMR.paramName p2
      b1 = T.dropWhileEnd (/= '/') n1
      b2 = T.dropWhileEnd (/= '/') n2
  in case b1 `compare` b2 of
       EQ -> n1 `compare` n2
       x -> x

main = do
  o <- execParser opts'
  T.putStrLn $ "API version: " <> TMR.apiVersion
  rdr <- createAndConnect (oUri o) (oListen o)

  params <- TMR.paramList rdr
  forM_ (sortBy compareParams params) $ \param -> do
    setSGR [SetColor Foreground Vivid Blue]
    putStr $ show param
    setSGR [Reset]
    T.putStrLn $ " - " <> TMR.paramName param
    let typ = TMR.displayParamType $ TMR.paramType param
    eth <- try $ TMR.paramGetString rdr param
    case eth of
      Left err -> do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ "  " ++ show (TMR.meStatus err)
        setSGR [Reset]
      Right txt -> do
        setSGR [SetColor Foreground Dull Green]
        T.putStr $ "  " <> txt
        setSGR [Reset]
        T.putStr $ " :: " <> typ
        case TMR.paramUnits param of
          Nothing -> T.putStrLn ""
          Just units -> T.putStrLn $ " (" <> units <> ")"
  TMR.destroy rdr
