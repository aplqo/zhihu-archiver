{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Column (Column (..)) where

import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Network.HTTP.Req
import ZhArchiver.Author
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH (deriveHasImage)
import ZhArchiver.Item
import ZhArchiver.Item.AnsOrArt (AnsOrArt)
import ZhArchiver.RawParser.TH
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri hiding (https)
import ZhArchiver.Types

data Column = Column
  { coId, coTitle :: Text,
    coUpdated, coCreated :: Time,
    coVote :: Int64,
    coDescription, coIntro :: Maybe Content,
    coAuthor :: Author,
    coImage :: Image,
    coRawData :: JSON.Value
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''Column

deriveHasImage ''Column ['coAuthor, 'coDescription, 'coIntro, 'coImage]

instance Item Column where
  type IId Column = Text
  type Signer Column = ()
  fetchRaw _ cid =
    Raw . responseBody
      <$> req
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "columns" /: cid)
        NoReqBody
        jsonResponse
        ("include" =: ("created,intro" :: Text))

instance ZhData Column where
  parseRaw (Raw v) =
    $( rawParser
         'Column
         [ ('coId, foStock "id"),
           ('coTitle, foStock "title"),
           ('coCreated, FoParse "created" poTime),
           ('coUpdated, FoParse "updated" poTime),
           ('coVote, foStock "voteup_count"),
           ('coDescription, FoParse "description" poContentMaybe),
           ('coIntro, FoParse "intro" poContentMaybe),
           ('coAuthor, FoParse "author" poAuthor),
           ('coImage, FoParse "image_url" poImage),
           ('coRawData, FoRaw)
         ]
     )
      v

instance ItemContainer Column AnsOrArt where
  type ICOpt Column AnsOrArt = Bool
  type ICSigner Column AnsOrArt = ()
  fetchItemsRaw pin _ Column {coId = cid} =
    if pin
      then do
        sp1 <- $(apiPath "columns" "items") cid
        fmap Raw <$> reqPaging (httpsURI sp1 [])
      else do
        sp2 <- $(apiPath "columns" "pinned-items") cid
        fmap Raw <$> reqPaging (httpsURI sp2 [])
