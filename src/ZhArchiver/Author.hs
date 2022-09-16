{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Author (Author (..), parseAuthor) where

import Data.Aeson hiding (Value)
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)
import ZhArchiver.Image
import ZhArchiver.Image.TH

data Author = Author
  { auId, auUrlToken, auName, auHeadline :: Text,
    auAvatar :: Image
  }
  deriving (Generic, Show)

instance FromJSON Author where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance ToJSON Author where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}

deriveHasImage ''Author ['auAvatar]

parseAuthor :: JSON.Value -> Parser (Maybe Author)
parseAuthor =
  withObject
    "author"
    ( \o -> do
        uid <- o .: "id"
        if uid == "0" -- anonymous
          then return Nothing
          else do
            uToken <- o .: "url_token"
            name <- o .: "name"
            headline <- o .: "headline"
            avatarUrl <- o .: "avatar_url"
            return
              ( Just
                  Author
                    { auId = uid,
                      auUrlToken = uToken,
                      auName = name,
                      auHeadline = headline,
                      auAvatar = Image {imgUrl = avatarUrl, imgRef = Nothing}
                    }
              )
    )
