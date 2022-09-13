{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ZhArchiver.Request.Paging (reqPaging, reqPagingSign) where

import Data.Aeson hiding (Value, defaultOptions)
import qualified Data.Aeson as JSON
import Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req
import Text.URI

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
          p <- o .: "paging"
          return (APIResponse res p)
      )

reqPagingSign :: URI -> (URI -> IO (Option 'Https)) -> IO [JSON.Value]
reqPagingSign u sig = iter (1 :: Int) 0 u
  where
    iter page cnt uri =
      do
        sigOp <- sig uri
        let (url, op) = fromJust $ useHttpsURI uri
        p <-
          runReq defaultHttpConfig $
            responseBody
              <$> req GET url NoReqBody (jsonResponse @APIResponse) (op <> sigOp)

        let l = cnt + length (rawResult p)
        putStrLn (concat ["Got page ", show page, ", ", show l, "/", show (paging p >>= totals), " items"])

        case paging p of
          Just pa | not (is_end pa) -> (rawResult p ++) <$> (mkURI (next pa) >>= iter (page + 1) l)
          _ -> return (rawResult p)

reqPaging :: URI -> IO [JSON.Value]
reqPaging uri = reqPagingSign uri (const (return mempty))