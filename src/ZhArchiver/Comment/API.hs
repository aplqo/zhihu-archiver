{-# LANGUAGE QuasiQuotes #-}

module ZhArchiver.Comment.API where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import qualified Data.Text as T
import Text.URI
import Text.URI.QQ
import ZhArchiver.Request.Paging

data SourceType
  = Article
  | Answer

getRootCommentRaw :: SourceType -> Int -> IO [JSON.Value]
getRootCommentRaw st sid =
  do
    sp <- mkPathPiece (T.pack (show sid))
    reqPaging
      ( httpsURI
          wwwHost
          ( [pathPiece|api|]
              :| [ [pathPiece|v4|],
                   [pathPiece|comment_v5|],
                   case st of
                     Article -> [pathPiece|articles|]
                     Answer -> [pathPiece|answers|],
                   sp,
                   [pathPiece|root_comment|]
                 ]
          )
          []
      )

getChildCommentRaw :: Int -> IO [JSON.Value]
getChildCommentRaw sid =
  do
    sp <- mkPathPiece (T.pack (show sid))
    reqPaging
      ( httpsURI
          wwwHost
          ( [pathPiece|api|]
              :| [ [pathPiece|v4|],
                   [pathPiece|comment_v5|],
                   sp,
                   [pathPiece|childComment|]
                 ]
          )
          []
      )