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

suppressUri :: Either TMR.MercuryException a -> Either TMR.MercuryException a
suppressUri (Left exc) = Left exc { TMR.meUri = T.empty }
suppressUri x = x

check :: (Read a, Show a, Eq a) => TestState -> IO a -> IO a
check ts f = do
  eth' <- try f
  let eth = suppressUri eth'
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
            listener <- TMR.opcodeListener hTransport
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

setRegionAndPower :: TMR.Reader -> IO ()
setRegionAndPower rdr = do
  -- pwr <- TMR.paramGetRadioPowerMax rdr
  TMR.paramSetBasics rdr TMR.REGION_NA2 2200 TMR.sparkFunAntennas
  TMR.paramSetTagReadDataRecordHighestRssi rdr True

readUser =
  TMR.TagOp_GEN2_ReadData
  { TMR.opBank = TMR.GEN2_BANK_USER
  , TMR.opExtraBanks = []
  , TMR.opWordAddress = 0
  , TMR.opLen = 32
  }

emptyUserDataFilter :: TMR.TagFilter
emptyUserDataFilter = TMR.TagFilterGen2
  { TMR.tfInvert = False
  , TMR.tfFilterOn = TMR.FilterOnBank TMR.GEN2_BANK_USER
  , TMR.tfBitPointer = 0
  , TMR.tfMaskBitLength = 16
  , TMR.tfMask = B.pack [0, 0]
  }

testParams :: TestFunc
testParams rdr ts = do
  params <- check ts $ TMR.paramList rdr
  let params' = TMR.PARAM_URI `delete` params -- because URI will be different
  forM_ params' $ \param -> do
    check ts $ TMR.paramGetString rdr param

testRead :: TestFunc
testRead rdr ts = do
  setRegionAndPower rdr

  tags <- TMR.read rdr 1000
  check ts $ return $ length tags
  forM_ tags $ \tag -> do
    check ts $ return tag { TMR.trTimestamp = 0 }

testReadUser :: TestFunc
testReadUser rdr ts = do
  setRegionAndPower rdr
  TMR.paramSetReadPlanTagop rdr (Just readUser)

  tags <- TMR.read rdr 1000
  check ts $ return $ length tags
  forM_ tags $ \tag -> do
    check ts $ return tag { TMR.trTimestamp = 0 }

testWrite :: TestFunc
testWrite rdr ts = do
  setRegionAndPower rdr
  TMR.paramSetReadPlanFilter rdr (Just emptyUserDataFilter)

  tags <- TMR.read rdr 1000
  check ts $ return $ length tags
  let trd = maximumBy (comparing TMR.trRssi) tags
  check ts $ return trd { TMR.trTimestamp = 0 }

  let epcFilt = TMR.TagFilterEPC (TMR.trTag trd)
      words = TMR.packBytesIntoWords "I am Groot"
      opWrite = TMR.TagOp_GEN2_WriteData
                { TMR.opBank = TMR.GEN2_BANK_USER
                , TMR.opWordAddress = 0
                , TMR.opData = words
                }
  check ts $ TMR.executeTagOp rdr opWrite (Just epcFilt)

  TMR.paramSetReadPlanFilter rdr Nothing
  TMR.paramSetReadPlanTagop rdr (Just readUser)
  tags2 <- TMR.read rdr 1000
  check ts $ return $ length tags2
  forM_ tags2 $ \tag -> do
    check ts $ return tag { TMR.trTimestamp = 0 }

tests :: [(String, TestFunc)]
tests =
  [ ("params", testParams)
  , ("read", testRead)
  , ("readUser", testReadUser)
  , ("write", testWrite)
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
