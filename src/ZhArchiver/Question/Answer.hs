{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ZhArchiver.Question.Answer where

import Data.Aeson hiding (Value, defaultOptions)
import qualified Data.Aeson as JSON
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Req
import Text.URI

data Paging = Paging
  { is_end :: Bool,
    page :: Int,
    next :: Text
  }
  deriving (Generic)

instance FromJSON Paging where
  parseJSON = genericParseJSON JSON.defaultOptions

data APIResponse = APIResponse
  { rawResult :: [JSON.Value],
    paging :: Paging
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

getRawJSON :: Int -> IO [JSON.Value]
getRawJSON qid =
  iter
    0
    ( https "api.zhihu.com" /: "questions" /: T.pack (show qid) /: "feeds",
      "include" =: ("data[*].is_normal,admin_closed_comment,reward_info,is_collapsed,annotation_action,annotation_detail,collapse_reason,is_sticky,collapsed_by,suggest_edit,comment_count,can_comment,content,editable_content,attachment,voteup_count,reshipment_settings,comment_permission,created_time,updated_time,review_info,relevant_info,question,excerpt,is_labeled,paid_info,paid_info_content,reaction_instruction,relationship.is_authorized,is_author,voting,is_thanked,is_nothelp,is_recognized;data[*].mark_infos[*].url;data[*].author.follower_count,vip_info,badge[*].topics;data[*].settings.table_of_content.enabled" :: Text)
    )
  where
    iter num (url, opt) = do
      resp <-
        runReq defaultHttpConfig $
          responseBody
            <$> req
              GET
              url
              NoReqBody
              (jsonResponse @APIResponse)
              opt
      let p = paging resp
          n = length (rawResult resp)
      putStrLn ("Got page " ++ show (page p) ++ " " ++ show num ++ " answers.")
      if is_end p
        then return (rawResult resp)
        else
          (rawResult resp ++)
            <$> (mkURI (next p) >>= (iter (num + n) . fromJust . useHttpsURI))