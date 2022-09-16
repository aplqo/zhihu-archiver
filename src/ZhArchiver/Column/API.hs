{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Column.API where

import Control.Monad.Catch (MonadThrow)
import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import Data.Text (Text)
import Network.HTTP.Req (MonadHttp)
import Text.URI
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri

getItemsRaw :: (MonadHttp m, MonadThrow m) => Text -> m [JSON.Value]
getItemsRaw cid =
  do
    sp <- $(apiPath "columns" "items") cid
    reqPaging
      (httpsURI sp [])

getPinnedItemsRaw :: (MonadHttp m, MonadThrow m) => Text -> m [JSON.Value]
getPinnedItemsRaw cid =
  do
    sp <- $(apiPath "columns" "pinned-items") cid
    reqPaging
      (httpsURI sp [])