{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.People.API where

import Control.Monad.Catch (MonadThrow)
import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import Data.Text (Text)
import Network.HTTP.Req
import Text.URI
import Text.URI.QQ
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri
import ZhArchiver.Request.Zse96V3

getActivityRaw :: (MonadHttp m, MonadThrow m) => Text -> m [JSON.Value]
getActivityRaw uid =
  do
    sp <- $(apiPath "web_moments" "activities") uid
    reqPaging
      (httpsURI sp [])

getAnswersRaw :: (MonadHttp m, MonadThrow m) => ZseState -> Text -> m [JSON.Value]
getAnswersRaw zs uid =
  do
    sp <- $(apiPath "members" "answers") uid
    reqPagingSign
      ( httpsURI
          sp
          [ QueryParam [queryKey|include|] [queryValue|data[*].is_normal,admin_closed_comment,reward_info,is_collapsed,annotation_action,annotation_detail,collapse_reason,collapsed_by,suggest_edit,comment_count,can_comment,content,editable_content,attachment,voteup_count,reshipment_settings,comment_permission,mark_infos,created_time,updated_time,review_info,excerpt,is_labeled,label_info,relationship.is_authorized,voting,is_author,is_thanked,is_nothelp,is_recognized;data[*].vessay_info;data[*].author.badge[?(type=best_answerer)].topics;data[*].author.vip_info;data[*].question.has_publishing_draft,relationship|],
            QueryParam [queryKey|limit|] [queryValue|20|],
            QueryParam [queryKey|offset|] [queryValue|0|]
          ]
      )
      (zse96 zs)

getArticlesRaw :: (MonadHttp m, MonadThrow m) => ZseState -> Text -> m [JSON.Value]
getArticlesRaw zs uid =
  do
    sp <- $(apiPath "members" "articles") uid
    reqPagingSign
      ( httpsURI
          sp
          [QueryParam [queryKey|include|] [queryValue|data[*].comment_count,suggest_edit,is_normal,thumbnail_extra_info,thumbnail,can_comment,comment_permission,admin_closed_comment,content,voteup_count,created,updated,upvoted_followees,voting,review_info,is_labeled,label_info;data[*].vessay_info;data[*].author.badge[?(type=best_answerer)].topics;data[*].author.vip_info;|]]
      )
      (zse96 zs)

getColumnRaw :: (MonadHttp m, MonadThrow m) => Text -> m [JSON.Value]
getColumnRaw uid =
  do
    sp <- $(apiPath "members" "column-contributions") uid
    reqPaging
      (httpsURI sp [])

getPinsRaw :: (MonadHttp m, MonadThrow m) => Text -> m [JSON.Value]
getPinsRaw uid =
  do
    sp <- $(apiPath "pins" "moments") uid
    reqPaging
      (httpsURI sp [])

getCollectionsRaw :: (MonadHttp m, MonadThrow m) => Text -> m [JSON.Value]
getCollectionsRaw uid =
  do
    sp <- $(apiPath "people" "collections") uid
    reqPaging
      ( httpsURI
          sp
          [QueryParam [queryKey|include|] [queryValue|data[*].updated_time,answer_count,follower_count,creator,description,is_following,comment_count,created_time;data[*].creator.vip_info|]]
      )

getFollowingFavlistsRaw :: (MonadHttp m, MonadThrow m) => Text -> m [JSON.Value]
getFollowingFavlistsRaw uid =
  do
    sp <- $(apiPath "members" "following-favlists") uid
    reqPaging
      ( httpsURI
          sp
          [QueryParam [queryKey|include|] [queryValue|data[*].updated_time,answer_count,follower_count,creator,description,is_following,comment_count,created_time|]]
      )