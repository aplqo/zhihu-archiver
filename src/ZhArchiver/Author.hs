{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Author
  ( Author (..),
  )
where

import Data.Aeson hiding (Value)
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import ZhArchiver.Image
import ZhArchiver.Image.TH (deriveHasImage)
import ZhArchiver.Raw
import ZhArchiver.Raw.Parser.TH
import ZhArchiver.Raw.Parser.Util

data Author = Author
  { auId, auUrlToken, auName, auHeadline :: Text,
    auAvatar :: Image
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 2} ''Author

deriveHasImage ''Author [('auAvatar, "avatar")]

instance FromRaw Author where
  parseRaw =
    $( rawParser
         'Author
         [ ('auId, FoParse "id" PoStock),
           ('auUrlToken, FoParse "url_token" PoStock),
           ('auName, FoParse "name" PoStock),
           ('auHeadline, FoParse "headline" PoStock),
           ('auAvatar, FoParse "avatar_url" poImage)
         ]
     )

instance FromRaw (Maybe Author) where
  parseRaw = fmap (unlessMaybe ((== "0") . auId)) . parseRaw