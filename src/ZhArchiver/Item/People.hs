{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.People (People (..), CollType (..)) where

import Data.Aeson hiding (Value)
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import System.FilePath
import Text.URI
import Text.URI.QQ
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH (deriveHasImage)
import ZhArchiver.Item
import ZhArchiver.Item.Answer (Answer)
import ZhArchiver.Item.Article
import ZhArchiver.Item.Article.Parser
import ZhArchiver.Item.Collection
import ZhArchiver.Item.Column
import ZhArchiver.Item.Pin
import ZhArchiver.Progress
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

deriveHasImage
  ''People
  [ ('pDescription, "description"),
    ('pAvatar, "avatar"),
    ('pCover, "cover")
  ]

instance ShowId People where
  showType = const "people"
  showId People {pUrlToken = t} = T.unpack t

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
  type ICOpt People Answer = ()
  type ICSigner People Answer = ZseState
  fetchItemsRaw cli _ zs (People {pUrlToken = uid}) = do
    sp <- $(apiPath "members" "answers") uid
    fmap Raw
      <$> reqPagingSign
        cli
        ( httpsURI
            sp
            [ QueryParam [queryKey|include|] [queryValue|data[*].is_normal,admin_closed_comment,reward_info,is_collapsed,annotation_action,annotation_detail,collapse_reason,collapsed_by,suggest_edit,comment_count,can_comment,content,editable_content,attachment,voteup_count,reshipment_settings,comment_permission,mark_infos,created_time,updated_time,review_info,excerpt,is_labeled,label_info,relationship.is_authorized,voting,is_author,is_thanked,is_nothelp,is_recognized;data[*].vessay_info;data[*].author.badge[?(type=best_answerer)].topics;data[*].author.vip_info;data[*].question.has_publishing_draft,relationship|],
              QueryParam [queryKey|limit|] [queryValue|20|],
              QueryParam [queryKey|offset|] [queryValue|0|]
            ]
        )
        (zse96 zs)

instance ItemContainer People Article where
  type ICOpt People Article = ()
  type ICSigner People Article = ZseState
  fetchItemsRaw cli _ zs (People {pUrlToken = uid}) = do
    sp <- $(apiPath "members" "articles") uid
    fmap Raw
      <$> reqPagingSign
        cli
        ( httpsURI
            sp
            [QueryParam [queryKey|include|] [queryValue|data[*].comment_count,suggest_edit,is_normal,thumbnail_extra_info,thumbnail,can_comment,comment_permission,admin_closed_comment,content,voteup_count,created,updated,upvoted_followees,voting,review_info,is_labeled,label_info;data[*].vessay_info;data[*].author.badge[?(type=best_answerer)].topics;data[*].author.vip_info;|]]
        )
        (zse96 zs)
  parseRawChild _ (Raw v) = $(mkArticleParser False) v

data PeopleColumn = PCol {pcColumn :: Column, pcRawData :: JSON.Value}
  deriving (Show)

instance ShowId PeopleColumn where
  showType = const "column"
  showId p = showId (pcColumn p)

deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''PeopleColumn

deriveHasImage ''PeopleColumn [('pcColumn, "column")]

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
  type ICOpt People PeopleColumn = ()
  type ICSigner People PeopleColumn = ()
  fetchItemsRaw cli _ _ (People {pUrlToken = uid}) =
    do
      sp <- $(apiPath "members" "column-contributions") uid
      fmap Raw
        <$> reqPaging
          cli
          ( httpsURI
              sp
              [QueryParam [queryKey|include|] [queryValue|data[*].column.intro,followers,articles_count,voteup_count,items_count,description,created|]]
          )

instance ItemContainer People Pin where
  type ICOpt People Pin = ()
  type ICSigner People Pin = ()
  fetchItemsRaw cli _ _ (People {pUrlToken = uid}) = do
    sp <- $(apiPath "pins" "moments") uid
    fmap Raw
      <$> reqPaging
        cli
        (httpsURI sp [])
  parseRawChild _ (Raw c) =
    withObject
      "people.pin"
      ( \o -> do
          v <- o .: "target" >>= parseRaw . Raw
          return v {pinRawData = c}
      )
      c

data CollType
  = CotCreated
  | CotLiked

instance ItemContainer People Collection where
  type ICOpt People Collection = CollType
  type ICSigner People Collection = ()
  fetchItemsRaw cli typ _ People {pUrlToken = uid} =
    fmap Raw <$> case typ of
      CotCreated ->
        do
          sp <- $(apiPath "people" "collections") uid
          reqPaging
            cli
            ( httpsURI
                sp
                [QueryParam [queryKey|include|] [queryValue|data[*].updated_time,answer_count,follower_count,creator,description,is_following,comment_count,created_time;data[*].creator.vip_info|]]
            )
      CotLiked ->
        do
          sp <- $(apiPath "members" "following-favlists") uid
          reqPaging
            cli
            ( httpsURI
                sp
                [QueryParam [queryKey|include|] [queryValue|data[*].updated_time,answer_count,follower_count,creator,description,is_following,comment_count,created_time|]]
            )
  saveItems p t s =
    traverse_
      ( saveData
          ( p </> showId s
              </> ( case t of
                      CotCreated -> "collection"
                      CotLiked -> "following-favlist"
                  )
          )
      )
