{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Author (Author (..), parseAuthor) where

import Data.Aeson hiding (Value)
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import ZhArchiver.Image
import ZhArchiver.Image.TH

data Author = Author
  { auId, auUrlToken, auName, auHeadline :: Text,
    auAvatar :: Image
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''Author

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
