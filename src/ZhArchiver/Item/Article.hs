{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Article (ArtId (..), Article (..)) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Bifunctor
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import ZhArchiver.Author
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH (deriveHasImage)
import ZhArchiver.Item
import ZhArchiver.Item.Article.Parser
import ZhArchiver.Progress
import ZhArchiver.Raw
import ZhArchiver.Types

newtype ArtId = ArtId Int
  deriving newtype (Show, FromJSON, ToJSON)

data Article = Article
  { artId :: ArtId,
    artTitle :: Text,
    artImage, artTitleImage :: Maybe Image,
    artAuthor :: Author,
    artCreate, artUpdate :: Time,
    artVote :: Int64,
    artContent :: Content,
    artCommentCount :: Int,
    artComment :: [Comment]
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''Article

instance ShowId Article where
  showType = const "article"
  showId Article {artId = ArtId a} = show a

instance FromRaw Article where
  parseRaw = $(mkArticleParser True)

instance ZhData Article

instance Item Article where
  type IId Article = ArtId
  type Signer Article = ()

  fetchRaw _ (ArtId i) =
    Raw . responseBody
      <$> req
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "articles" /: T.pack (show i))
        NoReqBody
        jsonResponse
        mempty

instance Commentable Article where
  hasComment a = artCommentCount a /= 0
  attachComment cli a =
    bimap (\c -> a {artComment = c}) (singletonRm "comment" . packLeaf)
      <$> fetchComment
        (pushHeader "comments" cli)
        StArticle
        (T.pack (show (artId a)))

deriveHasImage
  ''Article
  [ ('artAuthor, "author"),
    ('artImage, "image"),
    ('artTitleImage, "title_image"),
    ('artContent, "content"),
    ('artComment, "comment")
  ]