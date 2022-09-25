{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Article (IId (..), Article (..)) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Bifunctor
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import Text.Pandoc.Builder
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

data Article = Article
  { artId :: IId Article,
    artTitle :: Text,
    artImage, artTitleImage :: Maybe Image,
    artAuthor :: Author,
    artCreate, artUpdate :: Time,
    artVote :: Int64,
    artContent :: Content,
    artCommentCount :: Int,
    artComment :: Maybe [Comment]
  }

deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3} ''Article

instance ShowId Article where
  showType = const "article"
  showId Article {artId = ArtId a} = show a

instance ShowName Article where
  showName = T.unpack . artTitle

instance FromRaw Article where
  parseRaw = $(mkArticleParser True)

instance ZhData Article

instance Item Article where
  newtype IId Article = ArtId Int64
    deriving newtype (Show, FromJSON, ToJSON)
  type Signer Article = ()

  fetchRaw _ (ArtId i) =
    Raw . responseBody
      <$> req
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "articles" /: T.pack (show i))
        NoReqBody
        jsonResponse
        mempty

deriving instance (Show Article)

instance Commentable Article where
  hasComment a = artCommentCount a /= 0
  attachComment cli a =
    bimap (\c -> a {artComment = Just c}) (singletonRm "comment" . packLeaf)
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

instance HasContent Article where
  convertContent p v =
    Just . setTitle (text (artTitle v))
      <$> contentToPandoc p (artContent v)