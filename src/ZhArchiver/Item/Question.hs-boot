module ZhArchiver.Item.Question (QId) where

import Data.Aeson

data QId

instance Show QId

instance FromJSON QId

instance ToJSON QId