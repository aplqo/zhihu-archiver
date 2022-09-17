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
    withObject
      "people"
      ( \o ->
          do
            pid <- o .: "id"
            utoken <- o .: "url_token"
            name <- o .: "name"
            headline <- (\t -> if T.null t then Nothing else Just t) <$> o .: "headline"
            descrip <-
              ( \t ->
                  if T.null t
                    then Nothing
                    else Just Content {contHtml = t, contImages = emptyImgMap}
                )
                <$> o .: "description"
            avatar <- o .: "avatar_url"
            cover <- (\t -> if T.null t then Nothing else Just Image {imgUrl = t, imgRef = Nothing}) <$> o .: "cover_url"
            return
              People
                { pId = pid,
                  pUrlToken = utoken,
                  pName = name,
                  pHeadline = headline,
                  pDescription = descrip,
                  pAvatar = Image {imgUrl = avatar, imgRef = Nothing},
                  pCover = cover,
                  pRawData = v
                }
      )
      v