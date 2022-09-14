{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Question.API where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import qualified Data.Text as T
import Network.HTTP.Req (MonadHttp)
import Text.URI
import Text.URI.QQ
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri
import ZhArchiver.Types

getAnswersRaw :: MonadHttp m => Id -> m [JSON.Value]
getAnswersRaw qid =
  do
    p <- liftIO $ $(apiPath "questions" "feeds") (T.pack (show qid))
    reqPaging
      ( httpsURI
          p
          [QueryParam [queryKey|include|] [queryValue|data[*].is_normal,admin_closed_comment,reward_info,is_collapsed,annotation_action,annotation_detail,collapse_reason,is_sticky,collapsed_by,suggest_edit,comment_count,can_comment,content,editable_content,attachment,voteup_count,reshipment_settings,comment_permission,created_time,updated_time,review_info,relevant_info,question,excerpt,is_labeled,paid_info,paid_info_content,reaction_instruction,relationship.is_authorized,is_author,voting,is_thanked,is_nothelp,is_recognized;data[*].mark_infos[*].url;data[*].author.follower_count,vip_info,badge[*].topics;data[*].settings.table_of_content.enabled|]]
      )