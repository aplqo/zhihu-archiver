{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.People.API where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import Data.Text (Text)
import Network.HTTP.Req
import Text.URI
import Text.URI.QQ
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri

getActivityRaw :: MonadHttp m => Text -> m [JSON.Value]
getActivityRaw uid =
  do
    sp <- liftIO $ $(apiPath "web_moments" "activities") uid
    reqPaging
      (httpsURI sp [])

getColumnRaw :: MonadHttp m => Text -> m [JSON.Value]
getColumnRaw uid =
  do
    sp <- liftIO $ $(apiPath "members" "column-contributions") uid
    reqPaging
      (httpsURI sp [])

getPinsRaw :: MonadHttp m => Text -> m [JSON.Value]
getPinsRaw uid =
  do
    sp <- liftIO $ $(apiPath "pins" "moments") uid
    reqPaging
      (httpsURI sp [])

getCollectionsRaw :: MonadHttp m => Text -> m [JSON.Value]
getCollectionsRaw uid =
  do
    sp <- liftIO $ $(apiPath "people" "collections") uid
    reqPaging
      ( httpsURI
          sp
          [QueryParam [queryKey|include|] [queryValue|data[*].updated_time,answer_count,follower_count,creator,description,is_following,comment_count,created_time;data[*].creator.vip_info|]]
      )

getFollowingFavlistsRaw :: MonadHttp m => Text -> m [JSON.Value]
getFollowingFavlistsRaw uid =
  do
    sp <- liftIO $ $(apiPath "members" "following-favlists") uid
    reqPaging
      ( httpsURI
          sp
          [QueryParam [queryKey|include|] [queryValue|data[*].updated_time,answer_count,follower_count,creator,description,is_following,comment_count,created_time|]]
      )