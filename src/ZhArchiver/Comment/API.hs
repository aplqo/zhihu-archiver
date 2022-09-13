{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Comment.API where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import qualified Data.Text as T
import Text.URI
import Text.URI.QQ
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri

data SourceType
  = Article
  | Answer

getRootCommentRaw :: SourceType -> Int -> IO [JSON.Value]
getRootCommentRaw st sid =
  do
    sp <-
      $(pathTemplate [F "api", F "v4", F "comment_v5", P, T, F "root_comment"])
        ( case st of
            Article -> [pathPiece|articles|]
            Answer -> [pathPiece|answers|]
        )
        (T.pack (show sid))
    reqPaging
      (httpsURI sp [])

getChildCommentRaw :: Int -> IO [JSON.Value]
getChildCommentRaw sid =
  do
    sp <- $(apiPath "comment_v5" "childComment") (T.pack (show sid))
    reqPaging
      (httpsURI sp [])