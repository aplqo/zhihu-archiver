{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.People.API where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import Data.Text (Text)
import Text.URI
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