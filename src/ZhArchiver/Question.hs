{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module ZhArchiver.Question where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson hiding (Value)
import qualified Data.Aeson as JSON
import Data.Aeson.Types
import Data.Int
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Req
import qualified Network.HTTP.Req as R
import Text.URI
import Text.URI.QQ
import ZhArchiver.Author
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri hiding (https)
import ZhArchiver.Request.Zse96V3
import ZhArchiver.Types

data Question = Question
  { qId :: Id,
    qAuthor :: Maybe Author,
    qCreated, qUpdated :: Time,
    qContent :: Maybe Content,
    qComments :: [Comment],
    qRawData :: JSON.Value
  }
  deriving (Show, Generic)

instance FromJSON Question where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = tail}

instance ToJSON Question where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = tail}

fetchQuestionRaw :: MonadHttp m => ZseState -> Id -> m JSON.Value
fetchQuestionRaw zs qid =
  responseBody
    <$> reqCb
      GET
      (https "www.zhihu.com" /: "api" /: "v4" /: "questions" /: T.pack (show qid))
      NoReqBody
      jsonResponse
      ("include" =: ("author,description,is_anonymous;detail;comment_count;answer_count;excerpt" :: Text))
      (zse96 zs)

parseRawQuestion :: JSON.Value -> Question
parseRawQuestion =
  fromJust
    . parseMaybe
      ( \v ->
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
                return
                  Question
                    { qId = qid,
                      qAuthor = author,
                      qCreated = created,
                      qUpdated = updated,
                      qContent = content,
                      qComments = [],
                      qRawData = v
                    }
            )
            v
      )

fetchQuestion :: (MonadHttp m, MonadThrow m) => ZseState -> Id -> m Question
fetchQuestion zs qid = do
  q <- parseRawQuestion <$> fetchQuestionRaw zs qid
  c <- fetchComment StQuestion (T.pack (show qid))
  return q {qComments = c}

fetchAnswersRaw :: (MonadHttp m, MonadThrow m) => Id -> m [JSON.Value]
fetchAnswersRaw qid =
  do
    p <- $(apiPath "questions" "feeds") (T.pack (show qid))
    reqPaging
      ( httpsURI
          p
          [QueryParam [queryKey|include|] [queryValue|data[*].is_normal,admin_closed_comment,reward_info,is_collapsed,annotation_action,annotation_detail,collapse_reason,is_sticky,collapsed_by,suggest_edit,comment_count,can_comment,content,editable_content,attachment,voteup_count,reshipment_settings,comment_permission,created_time,updated_time,review_info,relevant_info,question,excerpt,is_labeled,paid_info,paid_info_content,reaction_instruction,relationship.is_authorized,is_author,voting,is_thanked,is_nothelp,is_recognized;data[*].mark_infos[*].url;data[*].author.follower_count,vip_info,badge[*].topics;data[*].settings.table_of_content.enabled|]]
      )

data Answer = Answer
  { aId :: Id,
    aAuthor :: Maybe Author,
    aCreated, aUpdated :: Time,
    aVoteUp :: Int64,
    aContent :: Content,
    aComment :: [Comment],
    aRawData :: JSON.Value
  }
  deriving (Show, Generic)

instance FromJSON Answer where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = tail}

instance ToJSON Answer where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = tail}

fetchAnswerRaw :: MonadHttp m => ZseState -> Id -> m JSON.Value
fetchAnswerRaw zs aid =
  responseBody
    <$> reqCb
      GET
      (R.https "www.zhihu.com" /: "api" /: "v4" /: "answers" /: T.pack (show aid))
      NoReqBody
      (jsonResponse @JSON.Value)
      ("include" =: ("content;comment_count;voteup_count" :: T.Text))
      (zse96 zs)

parseRawAnswer :: JSON.Value -> (Answer, Int, Int)
parseRawAnswer =
  fromJust
    . parseMaybe
      ( \v ->
          withObject
            "answer"
            ( \o -> do
                aid <- o .: "id"
                author <- o .: "author" >>= parseAuthor
                created <- o .: "created_time" >>= parseTime
                updated <- o .: "updated_time" >>= parseTime
                vote <- o .: "voteup_count"
                content <- o .: "content"

                qid <- o .: "question" >>= withObject "question" (.: "id")
                ccnt <- o .: "comment_count"

                return
                  ( Answer
                      { aId = aid,
                        aAuthor = author,
                        aCreated = created,
                        aUpdated = updated,
                        aVoteUp = vote,
                        aContent = Content {contHtml = content, contImages = emptyImgMap},
                        aComment = [],
                        aRawData = v
                      },
                    qid,
                    ccnt
                  )
            )
            v
      )
