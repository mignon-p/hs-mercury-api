module System.Hardware.MercuryApi where

import qualified Data.Text as T

import System.Hardware.MercuryApi.Generated

data MercuryException =
  MercuryException
  { meStatusType :: StatusType
  , meStatus     :: Status
  , meMessage    :: T.Text
  , meLocation   :: T.Text
  }
  deriving (Eq, Ord, Show, Read)
