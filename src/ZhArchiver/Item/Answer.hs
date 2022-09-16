{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Answer (AId (..), Answer (..)) where

import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import ZhArchiver.Author
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH
import ZhArchiver.Item
import ZhArchiver.Request.Zse96V3
import ZhArchiver.Types

newtype AId = AId Int
  deriving newtype (Show, FromJSON, ToJSON)

data Answer = Answer
  { aId :: AId,
    aAuthor :: Maybe Author,
    aQuestionId :: AId,
    aCreated, aUpdated :: Time,
    aVoteUp :: Int64,
    aContent :: Content,
    aCommentCount :: Int,
    aComment :: [Comment],
    aRawData :: JSON.Value
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = tail} ''Answer

instance Item Answer where
  type IId Answer = AId
  type Signer Answer = ZseState

  fetchRaw zs aid =
    Raw . responseBody
      <$> reqCb
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "answers" /: T.pack (show aid))
        NoReqBody
        jsonResponse
        ("include" =: ("content;comment_count;voteup_count" :: Text))
        (zse96 zs)

  parseRaw (Raw v) =
    withObject
      "answer"
      ( \o -> do
          aid <- o .: "id"
          author <- o .: "author" >>= parseAuthor
          qid <- o .: "question" >>= withObject "question" (.: "id")
          created <- o .: "created_time" >>= parseTime
          updated <- o .: "updated_time" >>= parseTime
          vote <- o .: "voteup_count"
          content <- o .: "content"
          ccnt <- o .: "comment_count"

          return
            Answer
              { aId = aid,
                aAuthor = author,
                aQuestionId = qid,
                aCreated = created,
                aUpdated = updated,
                aVoteUp = vote,
                aContent = Content {contHtml = content, contImages = emptyImgMap},
                aCommentCount = ccnt,
                aComment = [],
                aRawData = v
              }
      )
      v

instance Commentable Answer where
  commentCount = aCommentCount
  attachComment a =
    (\c -> a {aComment = c})
      <$> fetchComment StAnswer (T.pack (show (aId a)))

deriveHasImage ''Answer ['aAuthor, 'aContent, 'aComment]