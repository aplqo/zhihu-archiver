{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Collections.API where

import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import qualified Data.Text as T
import Network.HTTP.Req
import Text.URI
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri

getItemsRaw :: MonadHttp m => Int -> m [JSON.Value]
getItemsRaw cid = do
  sp <- liftIO $ $(apiPath "collections" "items") (T.pack (show cid))
  reqPaging (httpsURI sp [])