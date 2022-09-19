{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Item.AnsOrArt (AnsOrArt (..)) where

import Data.Aeson.TH
import Data.Aeson.Types
import Data.Bifunctor
import Data.Text (Text)
import ZhArchiver.Comment
import ZhArchiver.Image
import ZhArchiver.Item
import ZhArchiver.Item.Answer (Answer)
import ZhArchiver.Item.Article (Article)
import ZhArchiver.Progress
import ZhArchiver.Raw

data AnsOrArt
  = AoaArticle Article
  | AoaAnswer Answer
  deriving (Show)

instance ShowId AnsOrArt where
  showType = const "item"
  showId v =
    case v of
      AoaArticle a -> "art_" ++ showId a
      AoaAnswer a -> "ans_" ++ showId a

deriveJSON defaultOptions {constructorTagModifier = drop 3} ''AnsOrArt

instance FromRaw AnsOrArt where
  parseRaw v =
    withObject
      "CoItem"
      ( \o ->
          ((o .: "type") :: Parser Text) >>= \case
            "answer" -> AoaAnswer <$> parseRaw v
            "article" -> AoaArticle <$> parseRaw v
            _ -> error "unknown column item type"
      )
      v

instance ZhData AnsOrArt

instance HasImage AnsOrArt where
  fetchImage cli a =
    case a of
      AoaArticle ar -> AoaArticle <$> fetchImage cli ar
      AoaAnswer an -> AoaAnswer <$> fetchImage cli an

instance Commentable AnsOrArt where
  hasComment (AoaArticle a) = hasComment a
  hasComment (AoaAnswer a) = hasComment a

  attachComment cli (AoaArticle a) = first AoaArticle <$> attachComment cli a
  attachComment cli (AoaAnswer a) = first AoaAnswer <$> attachComment cli a
