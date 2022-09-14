{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ZhArchiver.Request.Paging (reqPaging, reqPagingSign) where

import Control.Monad.IO.Class
import Data.Aeson hiding (Value, defaultOptions)
import qualified Data.Aeson as JSON
import Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Network.HTTP.Client (Request)
import Network.HTTP.Req hiding (https)
import Text.URI
import ZhArchiver.Request.Uri

data Paging = Paging
  { is_end :: Bool,
    totals :: Maybe Int,
    next :: Text
  }
  deriving (Generic)

instance FromJSON Paging where
  parseJSON = genericParseJSON JSON.defaultOptions

data APIResponse = APIResponse
  { rawResult :: [JSON.Value],
    paging :: Maybe Paging
  }

instance FromJSON APIResponse where
  parseJSON =
    withObject
      "APIResponse"
      ( \o -> do
          res <- o .: "data"
          p <- o .:? "paging"
          return (APIResponse res p)
      )

reqPagingSign :: MonadHttp m => URI -> (Request -> m Request) -> m [JSON.Value]
reqPagingSign u sig = iter (1 :: Int) 0 u
  where
    iter page cnt uri =
      do
        let (url, op) = fromJust $ useHttpsURI (uri {uriScheme = Just https})
        p <-
          responseBody
            <$> reqCb GET url NoReqBody (jsonResponse @APIResponse) op sig

        let l = cnt + length (rawResult p)
        liftIO $ do
          putStr
            (concat ["\ESC[2K\ESC[G" {- clear line -}, "Got page ", show page, ", ", show l, maybe "" (\t -> "/" ++ show t) (paging p >>= totals), " items"])
          hFlush stdout

        case paging p of
          Just pa | not (is_end pa) -> (rawResult p ++) <$> (liftIO (mkURI (next pa)) >>= iter (page + 1) l)
          _ -> liftIO (putChar '\n') >> return (rawResult p)

reqPaging :: MonadHttp m => URI -> m [JSON.Value]
reqPaging uri = reqPagingSign uri pure