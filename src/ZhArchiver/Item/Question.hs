{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Question (QId (..), Question (..)) where

import Data.Aeson hiding (Value)
import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Req
import ZhArchiver.Author
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH
import ZhArchiver.Item
import ZhArchiver.Request.Zse96V3
import ZhArchiver.Types

newtype QId = QId Int
  deriving newtype (Show, FromJSON, ToJSON)

data Question = Question
  { qId :: QId,
    qAuthor :: Maybe Author,
    qCreated, qUpdated :: Time,
    qContent :: Maybe Content,
    qCommentCount :: Int,
    qComments :: [Comment],
    qRawData :: JSON.Value
  }
  deriving (Show, Generic)

instance FromJSON Question where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = tail}

instance ToJSON Question where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = tail}

instance Item Question where
  type IId Question = QId
  type Signer Question = ZseState
  fetchRaw zs (QId qid) =
    Raw . responseBody
      <$> reqCb
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "questions" /: T.pack (show qid))
        NoReqBody
        jsonResponse
        ("include" =: ("author,description,is_anonymous;detail;comment_count;answer_count;excerpt" :: Text))
        (zse96 zs)

  parseRaw (Raw v) =
    withObject
      "question"
      ( \o -> do
          qid <- o .: "id"
          author <- o .: "author" >>= parseAuthor
          created <- o .: "created" >>= parseTime
          updated <- o .: "updated_time" >>= parseTime
          content <-
            ( \d ->
                if T.null d
                  then Nothing
                  else Just (Content {contHtml = d, contImages = emptyImgMap})
              )
              <$> o .: "detail"
          ccnt <- o .: "comment_count"
          return
            Question
              { qId = qid,
                qAuthor = author,
                qCreated = created,
                qUpdated = updated,
                qContent = content,
                qCommentCount = ccnt,
                qComments = [],
                qRawData = v
              }
      )
      v

instance Commentable Question where
  commentCount = qCommentCount
  attachComment v =
    (\c -> v {qComments = c})
      <$> fetchComment StQuestion (T.pack (show (qId v)))

deriveHasImage ''Question ['qAuthor, 'qContent, 'qComments]