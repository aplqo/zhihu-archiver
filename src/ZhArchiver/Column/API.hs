{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Column.API where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import Data.Text (Text)
import Network.HTTP.Req (MonadHttp)
import Text.URI
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri

getItemsRaw :: MonadHttp m => Text -> m [JSON.Value]
getItemsRaw cid =
  do
    sp <- liftIO $ $(apiPath "columns" "items") cid
    reqPaging
      (httpsURI sp [])

getPinnedItemsRaw :: MonadHttp m => Text -> m [JSON.Value]
getPinnedItemsRaw cid =
  do
    sp <- liftIO $ $(apiPath "columns" "pinned-items") cid
    reqPaging
      (httpsURI sp [])