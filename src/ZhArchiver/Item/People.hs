{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.People (People (..)) where

import Data.Aeson hiding (Value)
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import Text.URI
import Text.URI.QQ
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH (deriveHasImage)
import ZhArchiver.Item
import ZhArchiver.Item.Answer (Answer)
import ZhArchiver.Item.Article (Article)
import ZhArchiver.Item.Column
import ZhArchiver.RawParser.TH
import ZhArchiver.RawParser.Util
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri hiding (https)
import ZhArchiver.Request.Zse96V3

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

instance ItemContainer People Answer where
  type ICSigner People Answer = ZseState
  fetchItemsRaw zs (People {pId = uid}) = do
    sp <- $(apiPath "members" "answers") uid
    fmap Raw
      <$> reqPagingSign
        ( httpsURI
            sp
            [ QueryParam [queryKey|include|] [queryValue|data[*].is_normal,admin_closed_comment,reward_info,is_collapsed,annotation_action,annotation_detail,collapse_reason,collapsed_by,suggest_edit,comment_count,can_comment,content,editable_content,attachment,voteup_count,reshipment_settings,comment_permission,mark_infos,created_time,updated_time,review_info,excerpt,is_labeled,label_info,relationship.is_authorized,voting,is_author,is_thanked,is_nothelp,is_recognized;data[*].vessay_info;data[*].author.badge[?(type=best_answerer)].topics;data[*].author.vip_info;data[*].question.has_publishing_draft,relationship|],
              QueryParam [queryKey|limit|] [queryValue|20|],
              QueryParam [queryKey|offset|] [queryValue|0|]
            ]
        )
        (zse96 zs)

instance ItemContainer People Article where
  type ICSigner People Article = ZseState
  fetchItemsRaw zs (People {pId = uid}) = do
    sp <- $(apiPath "members" "articles") uid
    fmap Raw
      <$> reqPagingSign
        ( httpsURI
            sp
            [QueryParam [queryKey|include|] [queryValue|data[*].comment_count,suggest_edit,is_normal,thumbnail_extra_info,thumbnail,can_comment,comment_permission,admin_closed_comment,content,voteup_count,created,updated,upvoted_followees,voting,review_info,is_labeled,label_info;data[*].vessay_info;data[*].author.badge[?(type=best_answerer)].topics;data[*].author.vip_info;|]]
        )
        (zse96 zs)

data PeopleColumn = PCol {pcColumn :: Column, pcRawData :: JSON.Value}
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''PeopleColumn

deriveHasImage ''PeopleColumn ['pcColumn]

instance ZhData PeopleColumn where
  parseRaw (Raw v) =
    $( rawParser
         'PCol
         [ ('pcColumn, FoParse "column" (PoBind [|fmap (\c -> c {coRawData = JSON.Null}) . parseRaw . Raw|])),
           ('pcRawData, FoRaw)
         ]
     )
      v

instance ItemContainer People PeopleColumn where
  type ICSigner People PeopleColumn = ()
  fetchItemsRaw _ (People {pId = uid}) =
    do
      sp <- $(apiPath "members" "column-contributions") uid
      fmap Raw
        <$> reqPaging
          ( httpsURI
              sp
              [QueryParam [queryKey|include|] [queryValue|data[*].column.intro,followers,articles_count,voteup_count,items_count,description,created|]]
          )
