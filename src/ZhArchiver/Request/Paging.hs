{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module ZhArchiver.Request.Paging (reqPaging, reqPagingSign) where

import Control.Monad.IO.Class
import Data.Aeson hiding (Value, defaultOptions)
import qualified Data.Aeson as JSON
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client (Request)
import Network.HTTP.Req hiding (https)
import Text.URI
import ZhArchiver.Progress
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
          res <- fromMaybe [] <$> o .:? "data"
          p <- o .:? "paging"
          return (APIResponse res p)
      )

reqPagingSign :: MonadHttp m => Cli -> URI -> (Request -> m Request) -> m [JSON.Value]
reqPagingSign cli u sig = iter (1 :: Int) 0 u
  where
    fixup url = if T.last url == '&' then T.dropEnd 1 url else url
    iter page cnt uri =
      do
        let (url, op) = fromJust $ useHttpsURI (uri {uriScheme = Just https})
        p <-
          responseBody
            <$> reqCb GET url NoReqBody (jsonResponse @APIResponse) op sig

        let l = cnt + length (rawResult p)
        liftIO $
          showProgress
            cli
            (concat ["page ", show page, "  item ", show l, maybe "" (\t -> "/" ++ show t) (paging p >>= totals)])

        case paging p of
          Just pa | not (is_end pa) -> (rawResult p ++) <$> (liftIO (mkURI (fixup $ next pa)) >>= iter (page + 1) l)
          _ -> liftIO (endProgress cli) >> return (rawResult p)

reqPaging :: MonadHttp m => Cli -> URI -> m [JSON.Value]
reqPaging cli uri = reqPagingSign cli uri pure