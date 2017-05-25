import Control.Concurrent
import Control.Exception
import Control.Monad
-- import qualified Data.ByteString as B
import Data.Int
import Data.List
import Data.Monoid
import Data.Ord
import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import Data.Word
import Options.Applicative
import System.Directory
import System.IO
import System.Info

import qualified System.Hardware.MercuryApi as TMR
import qualified System.Hardware.MercuryApi.Params as TMR
import qualified System.Hardware.MercuryApi.Testing as TMR

data TestDirection = Record | Playback

data TestState =
  TestState
  { tsDirection :: TestDirection
  , tsHandle :: Handle
  }

type TestFunc = TMR.Reader -> TestState -> IO ()

check :: (Read a, Show a, Eq a) => TestState -> IO a -> IO a
check ts f = do
  eth <- try f
  case tsDirection ts of
    Record -> hPutStrLn (tsHandle ts) (show eth)
    Playback -> do
      ln <- hGetLine (tsHandle ts)
      let expected = read ln :: (Read a => Either TMR.MercuryException a)
      when (expected /= eth) $ do
        putStrLn "expected:"
        putStrLn ln
        putStrLn "but got:"
        print eth
        fail "test failed"
  return $ case eth of
             Left exc -> throw exc -- only thrown if caller looks at result
             Right x -> x

runTest :: String -> TestDirection -> String -> TestFunc -> IO ()
runTest uri dir name func = do
  putStrLn $ "running test: " ++ name
  let fname = "tests/" ++ name
      transportFile = fname ++ ".transport"
      resultFile = fname ++ ".result"
  case dir of
    Record -> do
      withFile transportFile WriteMode $ \hTransport -> do
        withFile resultFile WriteMode $ \hResult -> do
          TMR.withReader (T.pack uri) $ \rdr -> do
            listener <- TMR.hexListener hTransport
            TMR.addTransportListener rdr listener
            TMR.paramSetTransportTimeout rdr 10000
            TMR.connect rdr
            func rdr (TestState dir hResult)
    Playback -> do
      absFile <- makeAbsolute transportFile
      withFile resultFile ReadMode $ \hResult -> do
        TMR.withReader (T.pack $ "test://" ++ absFile) $ \rdr -> do
          TMR.paramSetTransportTimeout rdr 10000
          TMR.connect rdr
          func rdr (TestState dir hResult)

tests :: [(String, TestFunc)]
tests =
  [ ("", undefined)
  ]

allTests = map fst tests

runTests :: String -> TestDirection -> [String] -> IO ()
runTests uri dir ts = do
  forM_ ts $ \t -> do
    let mf = t `lookup` tests
    case mf of
      Nothing -> fail $ "no test named " ++ t
      Just f -> runTest uri dir t f

defUri :: String
defUri = case os of
           "darwin" -> "tmr:///dev/cu.SLAB_USBtoUART"
           _ -> "tmr:///dev/ttyUSB0"

data Opts = Opts
  { oUri :: String
  , oRecord :: Bool
  , oTests :: [String]
  }

optUri :: Parser String
optUri = strOption (long "uri" <>
                    short 'u' <>
                    metavar "URI" <>
                    help ("Reader to connect to (default " ++ defUri ++ ")") <>
                    value defUri)

optRecord :: Parser Bool
optRecord = switch (long "record" <>
                    short 'R' <>
                    help "record a new test from a physical reader")

opts :: Parser Opts
opts = Opts
  <$> optUri
  <*> optRecord
  <*> many (argument str (metavar "TESTS..."))

opts' = info (helper <*> opts)
  ( fullDesc <>
    header "replay - automated tests that use a simulated reader" )

main = do
  TMR.registerTransportInit
  o <- execParser opts'

  let dir = if oRecord o then Record else Playback
  let ts = case oTests o of
             [] -> allTests
             xs -> xs

  runTests (oUri o) dir ts
