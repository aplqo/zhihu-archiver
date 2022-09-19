{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Column (IId (..), Column (..)) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import System.FilePath
import ZhArchiver.Author
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH (deriveHasImage)
import ZhArchiver.Item
import ZhArchiver.Item.AnsOrArt (AnsOrArt)
import ZhArchiver.Progress
import ZhArchiver.Raw
import ZhArchiver.RawParser.TH
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri hiding (https)
import ZhArchiver.Types

data Column = Column
  { coId :: IId Column,
    coTitle :: Text,
    coUpdated, coCreated :: Time,
    coVote :: Int64,
    coDescription, coIntro :: Maybe Content,
    coAuthor :: Author,
    coImage :: Image
  }

deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''Column

instance ShowId Column where
  showType = const "column"
  showId Column {coId = CoId c} = T.unpack c

instance Item Column where
  newtype IId Column = CoId Text
    deriving newtype (Show, FromJSON, ToJSON)
  type Signer Column = ()
  fetchRaw _ (CoId cid) =
    Raw . responseBody
      <$> req
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "columns" /: cid)
        NoReqBody
        jsonResponse
        ("include" =: ("created,intro" :: Text))

deriving instance (Show Column)

instance FromRaw Column where
  parseRaw =
    $( rawParser
         'Column
         [ ('coId, foStock "id"),
           ('coTitle, foStock "title"),
           ('coCreated, FoParse "created" poTime),
           ('coUpdated, FoParse "updated" poTime),
           ('coVote, foStock "voteup_count"),
           ('coDescription, FoParse "description" poContentMaybe),
           ('coIntro, FoParse "intro" poContentMaybe),
           ('coAuthor, foFromRaw "author"),
           ('coImage, FoParse "image_url" poImage)
         ]
     )

instance ZhData Column

deriveHasImage
  ''Column
  [ ('coAuthor, "author"),
    ('coDescription, "description"),
    ('coIntro, "intro"),
    ('coImage, "image")
  ]

instance ItemContainer Column AnsOrArt where
  type ICOpt Column AnsOrArt = Bool
  type ICSigner Column AnsOrArt = ()
  fetchItemsRaw cli pin _ Column {coId = CoId cid} =
    if pin
      then do
        sp2 <- $(apiPath "columns" "pinned-items") cid
        fmap Raw <$> reqPaging cli (httpsURI sp2 [])
      else do
        sp1 <- $(apiPath "columns" "items") cid
        fmap Raw <$> reqPaging cli (httpsURI sp1 [])
  childStorePath _ _ p op =
    p </> (if op then "pinned-item" else "item")
