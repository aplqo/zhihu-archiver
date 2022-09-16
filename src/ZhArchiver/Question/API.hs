{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module ZhArchiver.Question.API where

import Control.Monad.Catch (MonadThrow)
import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import qualified Data.Text as T
import Network.HTTP.Req
import qualified Network.HTTP.Req as R
import Text.URI
import Text.URI.QQ
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri
import ZhArchiver.Request.Zse96V3
import ZhArchiver.Types

getAnswersRaw :: (MonadHttp m, MonadThrow m) => Id -> m [JSON.Value]
getAnswersRaw qid =
  do
    p <- $(apiPath "questions" "feeds") (T.pack (show qid))
    reqPaging
      ( httpsURI
          p
          [QueryParam [queryKey|include|] [queryValue|data[*].is_normal,admin_closed_comment,reward_info,is_collapsed,annotation_action,annotation_detail,collapse_reason,is_sticky,collapsed_by,suggest_edit,comment_count,can_comment,content,editable_content,attachment,voteup_count,reshipment_settings,comment_permission,created_time,updated_time,review_info,relevant_info,question,excerpt,is_labeled,paid_info,paid_info_content,reaction_instruction,relationship.is_authorized,is_author,voting,is_thanked,is_nothelp,is_recognized;data[*].mark_infos[*].url;data[*].author.follower_count,vip_info,badge[*].topics;data[*].settings.table_of_content.enabled|]]
      )

getAnswerRaw :: MonadHttp m => ZseState -> Id -> m JSON.Value
getAnswerRaw zs aid =
  responseBody
    <$> reqCb
      GET
      (R.https "www.zhihu.com" /: "api" /: "v4" /: "answers" /: T.pack (show aid))
      NoReqBody
      (jsonResponse @JSON.Value)
      ("include" =: ("content;comment_count" :: T.Text))
      (zse96 zs)
