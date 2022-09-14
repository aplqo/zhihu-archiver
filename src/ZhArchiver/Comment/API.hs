{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Comment.API where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import qualified Data.Text as T
import Network.HTTP.Req (MonadHttp)
import Text.URI
import Text.URI.QQ
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri

data SourceType
  = Article
  | Answer
  | Collection
  | Question

getRootCommentRaw :: MonadHttp m => SourceType -> Int -> m [JSON.Value]
getRootCommentRaw st sid =
  do
    sp <-
      liftIO $
        $(pathTemplate [F "api", F "v4", F "comment_v5", P, T, F "root_comment"])
          ( case st of
              Article -> [pathPiece|articles|]
              Answer -> [pathPiece|answers|]
              Collection -> [pathPiece|collections|]
              Question -> [pathPiece|questions|]
          )
          (T.pack (show sid))
    reqPaging
      (httpsURI sp [])

getChildCommentRaw :: MonadHttp m => Int -> m [JSON.Value]
getChildCommentRaw sid =
  do
    sp <- liftIO $ $(apiPath "comment_v5" "childComment") (T.pack (show sid))
    reqPaging
      (httpsURI sp [])