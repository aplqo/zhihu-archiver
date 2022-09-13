{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.People.API where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import Data.Text (Text)
import Text.URI
import Text.URI.QQ
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri

getActivityRaw :: Text -> IO [JSON.Value]
getActivityRaw uid =
  do
    sp <- $(apiPath "web_moments" "activities") uid
    reqPaging
      (httpsURI sp [])

getColumnRaw :: Text -> IO [JSON.Value]
getColumnRaw uid =
  do
    sp <- $(apiPath "members" "column-contributions") uid
    reqPaging
      (httpsURI sp [])

getPinsRaw :: Text -> IO [JSON.Value]
getPinsRaw uid =
  do
    sp <- $(apiPath "pins" "moments") uid
    reqPaging
      (httpsURI sp [])

getCollectionsRaw :: Text -> IO [JSON.Value]
getCollectionsRaw uid =
  do
    sp <- $(apiPath "people" "collections") uid
    reqPaging
      ( httpsURI
          sp
          [QueryParam [queryKey|include|] [queryValue|data[*].updated_time,answer_count,follower_count,creator,description,is_following,comment_count,created_time;data[*].creator.vip_info|]]
      )

getFollowingFavlistsRaw :: Text -> IO [JSON.Value]
getFollowingFavlistsRaw uid =
  do
    sp <- $(apiPath "members" "following-favlists") uid
    reqPaging
      ( httpsURI
          sp
          [QueryParam [queryKey|include|] [queryValue|data[*].updated_time,answer_count,follower_count,creator,description,is_following,comment_count,created_time|]]
      )