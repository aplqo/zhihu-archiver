{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Article (ArtId (..), Article (..)) where

import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Req
import ZhArchiver.Author
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Item
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
    artComment :: [Comment],
    artRawData :: JSON.Value
  }
  deriving (Show, Generic)

instance FromJSON Article where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

instance ToJSON Article where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}

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

  parseRaw (Raw v) =
    withObject
      "article"
      ( \o -> do
          aid <- o .: "id"
          title <- o .: "title"
          image <- o .: "image_url" >>= parseImage
          titleImg <- o .: "title_image" >>= parseImage
          author <- fromJust <$> (o .: "author" >>= parseAuthor)
          create <- o .: "created" >>= parseTime
          update <- o .: "updated" >>= parseTime
          vote <- o .: "voteup_count"
          content <- o .: "content"
          ccnt <- o .: "comment_count"
          return
            Article
              { artId = aid,
                artTitle = title,
                artImage = image,
                artTitleImage = titleImg,
                artAuthor = author,
                artCreate = create,
                artUpdate = update,
                artVote = vote,
                artContent = Content {contHtml = content, contImages = emptyImgMap},
                artCommentCount = ccnt,
                artComment = [],
                artRawData = v
              }
      )
      v
    where
      parseImage va =
        ( \u ->
            if T.null u then Nothing else Just Image {imgUrl = u, imgRef = Nothing}
        )
          <$> parseJSON va

instance Commentable Article where
  commentCount = artCommentCount
  attachComment a = (\c -> a {artComment = c}) <$> fetchComment StArticle (T.pack (show a))