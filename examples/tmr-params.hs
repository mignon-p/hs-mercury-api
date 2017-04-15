{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Control.Monad
import qualified Data.Text as T
import qualified System.Hardware.MercuryApi as TMR

data DoWhat = Return | Throw | Retry deriving (Eq)

createAndConnect :: T.Text -> Int -> IO TMR.Reader
createAndConnect uri n = do
  rdr <- TMR.create uri
  eth <- try (TMR.connect rdr)
  let (doWhat, e) = case eth of
                      Left exc ->
                        case TMR.meStatus exc of
                          TMR.ERROR_TIMEOUT -> (Retry, exc)
                          TMR.ERROR_DEVICE_RESET -> (Retry, exc)
                          _ -> (Throw, exc)
                      _ -> (Return, undefined)
      doWhat' = if n <= 1 && doWhat == Retry
                then Throw
                else doWhat
  when (doWhat' /= Return) $ TMR.destroy rdr
  case doWhat' of
    Throw -> throw e
    Retry -> createAndConnect uri (n - 1)
    Return -> return rdr

main = do
  putStrLn "createAndConnect"
  rdr <- createAndConnect "tmr:///dev/ttyUSB0" 100
  putStrLn "paramList"
  params <- TMR.paramList rdr
  putStrLn "destroy"
  TMR.destroy rdr
  forM_ params $ \param -> do
    putStrLn $ show param ++ " - " ++ T.unpack (TMR.paramName param)
