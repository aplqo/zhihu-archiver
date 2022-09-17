{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Item.AnsOrArt (AnsOrArt (..)) where

import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)
import ZhArchiver.Comment
import ZhArchiver.Image
import ZhArchiver.Item
import ZhArchiver.Item.Answer (Answer)
import ZhArchiver.Item.Article (Article)

data AnsOrArt
  = AoaArticle Article
  | AoaAnswer Answer
  deriving (Show)

deriveJSON defaultOptions {constructorTagModifier = drop 3} ''AnsOrArt

instance ZhData AnsOrArt where
  parseRaw (Raw v) =
    withObject
      "CoItem"
      ( \o ->
          ((o .: "type") :: Parser Text) >>= \case
            "answer" -> AoaAnswer <$> parseRaw (Raw v)
            "article" -> AoaArticle <$> parseRaw (Raw v)
            _ -> error "unknown column item type"
      )
      v

instance HasImage AnsOrArt where
  fetchImage a =
    case a of
      AoaArticle ar -> AoaArticle <$> fetchImage ar
      AoaAnswer an -> AoaAnswer <$> fetchImage an

instance Commentable AnsOrArt where
  commentCount (AoaArticle a) = commentCount a
  commentCount (AoaAnswer a) = commentCount a

  attachComment (AoaArticle a) = AoaArticle <$> attachComment a
  attachComment (AoaAnswer a) = AoaAnswer <$> attachComment a
