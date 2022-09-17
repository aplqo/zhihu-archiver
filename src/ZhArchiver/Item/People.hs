{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.People (People (..)) where

import Data.Aeson hiding (Value)
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH
import ZhArchiver.Item
import ZhArchiver.RawParser.TH
import ZhArchiver.RawParser.Util

data People = People
  { pId, pUrlToken :: Text,
    pName :: Text,
    pHeadline :: Maybe Text,
    pDescription :: Maybe Content,
    pAvatar :: Image,
    pCover :: Maybe Image,
    pRawData :: JSON.Value
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = tail} ''People

deriveHasImage ''People ['pDescription, 'pAvatar, 'pCover]

instance Item People where
  type IId People = Text
  type Signer People = ()

  fetchRaw _ pid =
    Raw . responseBody
      <$> req
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "members" /: pid)
        NoReqBody
        jsonResponse
        ("include" =: ("allow_message,is_followed,is_following,is_org,is_blocking,employments,answer_count,follower_count,articles_count,gender,badge[?(type=best_answerer)].topics;description;cover_url" :: Text))

instance ZhData People where
  parseRaw (Raw v) =
    $( rawParser
         'People
         [ ('pId, FoParse "id" PoStock),
           ('pUrlToken, FoParse "url_token" PoStock),
           ('pName, FoParse "name" PoStock),
           ('pHeadline, FoParse "headline" (PoMap [|unlessMaybe T.null|])),
           ('pDescription, FoParse "description" poContentMaybe),
           ('pAvatar, FoParse "avatar_url" poImage),
           ('pCover, FoParse "cover_url" poImageMaybe),
           ('pRawData, FoRaw)
         ]
     )
      v