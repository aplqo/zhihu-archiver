{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ZhArchiver.Question.Answer where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import qualified Data.Text as T
import Text.URI
import Text.URI.QQ
import ZhArchiver.Request.Paging

getRawJSON :: Int -> IO [JSON.Value]
getRawJSON qid =
  do
    p <- mkPathPiece (T.pack (show qid))
    reqPaging
      URI
        { uriScheme = Just [scheme|https|],
          uriAuthority = Right (Authority Nothing [host|api.zhihu.com|] Nothing),
          uriPath = Just (False, [pathPiece|questions|] :| [p, [pathPiece|feeds|]]),
          uriQuery = [QueryParam [queryKey|include|] [queryValue|data[*].is_normal,admin_closed_comment,reward_info,is_collapsed,annotation_action,annotation_detail,collapse_reason,is_sticky,collapsed_by,suggest_edit,comment_count,can_comment,content,editable_content,attachment,voteup_count,reshipment_settings,comment_permission,created_time,updated_time,review_info,relevant_info,question,excerpt,is_labeled,paid_info,paid_info_content,reaction_instruction,relationship.is_authorized,is_author,voting,is_thanked,is_nothelp,is_recognized;data[*].mark_infos[*].url;data[*].author.follower_count,vip_info,badge[*].topics;data[*].settings.table_of_content.enabled|]],
          uriFragment = Nothing
        }