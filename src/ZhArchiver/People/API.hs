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