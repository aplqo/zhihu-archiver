{-# LANGUAGE DeriveGeneric #-}

module ZhArchiver.Content where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import ZhArchiver.Image

data Content = Content
  { contHtml :: Text,
    contImages :: ImgMap
  }
  deriving (Show, Generic)

instance FromJSON Content where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Content where
  toJSON = genericToJSON defaultOptions