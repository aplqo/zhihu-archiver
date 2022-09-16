{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ZhArchiver.Author where

import Data.Aeson hiding (Value)
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)

data Author = Author
  { auId, auUrlToken, auName, auHeadline, auAvatarUrl :: Text
  }
  deriving (Generic, Show)

instance FromJSON Author where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance ToJSON Author where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}

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
                      auAvatarUrl = avatarUrl
                    }
              )
    )
