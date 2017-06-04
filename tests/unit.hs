{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import Data.Monoid ((<>))
import qualified Data.Text as T
import Test.HUnit

import qualified System.Hardware.MercuryApi as TMR

helloWorld :: B.ByteString
helloWorld = "Hello, World!"

defacedBadFacade :: T.Text
defacedBadFacade = "DefacedBadFacade"

defacedBadFacadeBin :: B.ByteString
defacedBadFacadeBin = "\222\250\206\219\173\250\202\222"

-- http://www.adafruit.com/datasheets/CSN-A2%20User%20Manual.pdf
phrase :: B.ByteString
phrase = "thoroughly remove the uncommon words of anguish"

testDataHelpers :: Assertion
testDataHelpers = do
  assertEqual "TMR.packBytesIntoWords helloWorld"
    [18533,27756,28460,8279,28530,27748,8448]
    (TMR.packBytesIntoWords helloWorld)
  assertEqual "TMR.passwordToWords 0xbeadead1"
    [0xbead, 0xead1]
    (TMR.passwordToWords 0xbeadead1)

testHex :: Assertion
testHex = do
  assertEqual "TMR.bytesToHex defacedBadFacadeBin"
    "DEFACEDBADFACADE"
    (TMR.bytesToHex defacedBadFacadeBin)
  assertEqual "TMR.bytesToHexWithSpaces defacedBadFacadeBin"
    "de fa ce db ad fa ca de"
    (TMR.bytesToHexWithSpaces defacedBadFacadeBin)
  assertEqual "TMR.hexToBytes defacedBadFacade"
    (Just defacedBadFacadeBin)
    (TMR.hexToBytes defacedBadFacade)
  assertEqual "TMR.hexToBytes $ \"0x\" <> defacedBadFacade"
    (Just defacedBadFacadeBin)
    (TMR.hexToBytes $ "0x" <> defacedBadFacade)
  assertEqual "TMR.hexToBytes \"XXYYZZ\""
    Nothing
    (TMR.hexToBytes "XXYYZZ")
  assertEqual "TMR.hexToBytes \"0dd\""
    Nothing
    (TMR.hexToBytes "0dd")

testTimestamp :: Assertion
testTimestamp = do
  assertEqual "TMR.displayTimestamp 0"
    "1970-01-01T00:00:00.000Z"
    (TMR.displayTimestamp 0)
  assertEqual "TMR.displayTimestamp 123456789"
    "1970-01-02T10:17:36.789Z"
    (TMR.displayTimestamp 123456789)
  assertEqual "TMR.displayTimestamp 1513728000000"
    "2017-12-20T00:00:00.000Z"
    (TMR.displayTimestamp 1513728000000)

testData :: Assertion
testData = do
  assertEqual "TMR.displayData phrase"
    [ "74 68 6f 72 6f 75 67 68  6c 79 20 72 65 6d 6f 76  |thoroughly remov|"
    , "65 20 74 68 65 20 75 6e  63 6f 6d 6d 6f 6e 20 77  |e the uncommon w|"
    , "6f 72 64 73 20 6f 66 20  61 6e 67 75 69 73 68     |ords of anguish|"
    ]
    (TMR.displayData phrase)

tests :: Test
tests = TestList
  [ TestLabel "testDataHelpers" (TestCase testDataHelpers)
  , TestLabel "testHex" (TestCase testHex)
  , TestLabel "testTimestamp" (TestCase testTimestamp)
  , TestLabel "testData" (TestCase testData)
  ]

main = runTestTT tests
