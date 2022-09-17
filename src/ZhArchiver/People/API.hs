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

{-
getActivityRaw :: (MonadHttp m, MonadThrow m) => Text -> m [JSON.Value]
getActivityRaw uid =
  do
    sp <- $(apiPath "web_moments" "activities") uid
    reqPaging
      (httpsURI sp [])

getPinsRaw :: (MonadHttp m, MonadThrow m) => Text -> m [JSON.Value]
getPinsRaw uid =
  do
    sp <- $(apiPath "pins" "moments") uid
    reqPaging
      (httpsURI sp [])
      -}