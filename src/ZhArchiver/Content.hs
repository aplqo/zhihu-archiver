{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Content (Content (..)) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import ZhArchiver.Image

data Content = Content
  { contHtml :: Text,
    contImages :: ImgMap
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''Content

instance HasImage Content where
  fetchImage c = (\im -> c {contImages = im}) <$> getHtmlImages (contHtml c)