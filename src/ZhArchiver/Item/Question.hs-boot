{-# LANGUAGE FlexibleInstances #-}

module ZhArchiver.Item.Question (Question) where

import Data.Aeson
import ZhArchiver.Item

data Question

instance Item Question

instance FromJSON (IId Question)

instance ToJSON (IId Question)

instance Show (IId Question)