{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module ZhArchiver.Types (Id, mkId) where

import Data.Aeson

newtype Id = Id Int
  deriving (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

mkId :: Int -> Id
mkId = Id
