{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Collections.API where

import Control.Monad.Catch (MonadThrow)
import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import qualified Data.Text as T
import Network.HTTP.Req
import Text.URI
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri
import ZhArchiver.Types

getItemsRaw :: (MonadHttp m, MonadThrow m) => Id -> m [JSON.Value]
getItemsRaw cid = do
  sp <- $(apiPath "collections" "items") (T.pack (show cid))
  reqPaging (httpsURI sp [])