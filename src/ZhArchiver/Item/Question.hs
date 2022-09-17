{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Question (QId (..), Question (..)) where

import Data.Aeson hiding (Value)
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Network.HTTP.Req
import Text.URI
import Text.URI.QQ
import ZhArchiver.Author
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image.TH
import ZhArchiver.Item
import ZhArchiver.Item.Answer
import ZhArchiver.RawParser.TH
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri (apiPath, httpsURI)
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
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = tail} ''Question

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

instance ZhData Question where
  parseRaw (Raw v) =
    $( rawParser
         'Question
         [ ('qId, FoParse "id" PoStock),
           ('qAuthor, FoParse "author" poAuthorMaybe),
           ('qCreated, FoParse "created" poTime),
           ('qUpdated, FoParse "updated_time" poTime),
           ('qContent, FoParse "content" poContentMaybe),
           ('qCommentCount, FoParse "comment_count" PoStock),
           ('qComments, FoConst (listE [])),
           ('qRawData, FoRaw)
         ]
     )
      v

instance Commentable Question where
  commentCount = qCommentCount
  attachComment v =
    (\c -> v {qComments = c})
      <$> fetchComment StQuestion (T.pack (show (qId v)))

deriveHasImage ''Question ['qAuthor, 'qContent, 'qComments]

instance ItemContainer Question Answer where
  type ICSigner Question Answer = ZseState
  fetchItemsRaw zs Question {qId = qid} =
    do
      p <- $(apiPath "questions" "answers") (T.pack (show qid))
      fmap Raw
        <$> reqPagingSign
          ( httpsURI
              p
              [QueryParam [queryKey|include|] [queryValue|data[*].is_normal,admin_closed_comment,reward_info,is_collapsed,annotation_action,annotation_detail,collapse_reason,is_sticky,collapsed_by,suggest_edit,comment_count,can_comment,content,editable_content,attachment,voteup_count,reshipment_settings,comment_permission,created_time,updated_time,review_info,relevant_info,question,excerpt,is_labeled,paid_info,paid_info_content,reaction_instruction,relationship.is_authorized,is_author,voting,is_thanked,is_nothelp,is_recognized;data[*].mark_infos[*].url;data[*].author.follower_count,vip_info,badge[*].topics;data[*].settings.table_of_content.enabled|]]
          )
          (zse96 zs)
