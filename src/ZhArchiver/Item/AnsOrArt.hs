{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Item.AnsOrArt (AnsOrArt (..)) where

import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)
import System.FilePath
import ZhArchiver.Comment
import ZhArchiver.Image
import ZhArchiver.Item
import ZhArchiver.Item.Answer (Answer)
import ZhArchiver.Item.Article (Article)
import ZhArchiver.Progress

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
  saveData p v = case v of
    AoaArticle a ->
      withDirectory (p </> ("article_" ++ showId a)) $
        encodeFilePretty "info.json" v
    AoaAnswer a ->
      withDirectory (p </> ("answer_" ++ showId a)) $
        encodeFilePretty "info.json" v

instance HasImage AnsOrArt where
  fetchImage cli a =
    case a of
      AoaArticle ar -> AoaArticle <$> fetchImage cli ar
      AoaAnswer an -> AoaAnswer <$> fetchImage cli an

instance Commentable AnsOrArt where
  hasComment (AoaArticle a) = hasComment a
  hasComment (AoaAnswer a) = hasComment a

  attachComment cli (AoaArticle a) = AoaArticle <$> attachComment cli a
  attachComment cli (AoaAnswer a) = AoaAnswer <$> attachComment cli a
