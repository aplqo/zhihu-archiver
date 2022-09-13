{-# LANGUAGE QuasiQuotes #-}

module ZhArchiver.Column.API where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import Data.Text (Text)
import Text.URI
import Text.URI.QQ
import ZhArchiver.Request.Paging

getColItemsRaw :: Bool -> Text -> IO [JSON.Value]
getColItemsRaw pin cid =
  do
    sp <- mkPathPiece cid
    reqPaging
      ( httpsURI
          wwwHost
          ([pathPiece|api|] :| [[pathPiece|v4|], [pathPiece|columns|], sp, if pin then [pathPiece|pinned-items|] else [pathPiece|items|]])
          []
      )