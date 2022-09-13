{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Column.API where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import Data.Text (Text)
import Text.URI
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri

getItemsRaw :: Text -> IO [JSON.Value]
getItemsRaw cid =
  do
    sp <- $(apiPath "columns" "items") cid
    reqPaging
      (httpsURI sp [])

getPinnedItemsRaw :: Text -> IO [JSON.Value]
getPinnedItemsRaw cid =
  do
    sp <- $(apiPath "columns" "pinned-items") cid
    reqPaging
      (httpsURI sp [])