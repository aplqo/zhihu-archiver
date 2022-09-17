{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Column (Column (..)) where

import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.Foldable (traverse_)
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

deriveHasImage
  ''Column
  [ ('coAuthor, "author"),
    ('coDescription, "description"),
    ('coIntro, "intro"),
    ('coImage, "image")
  ]

instance ShowId Column where
  showType = const "column"
  showId Column {coId = c} = T.unpack c

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
  saveData p a =
    withDirectory (p </> showId a) $
      encodeFilePretty "info.json" a

instance ItemContainer Column AnsOrArt where
  type ICOpt Column AnsOrArt = Bool
  type ICSigner Column AnsOrArt = ()
  fetchItemsRaw cli pin _ Column {coId = cid} =
    if pin
      then do
        sp2 <- $(apiPath "columns" "pinned-items") cid
        fmap Raw <$> reqPaging (pushHeader "pinned-item" cli) (httpsURI sp2 [])
      else do
        sp1 <- $(apiPath "columns" "items") cid
        fmap Raw <$> reqPaging (pushHeader "item" cli) (httpsURI sp1 [])
  saveItems p op c =
    traverse_ (saveData (p </> showId c </> (if op then "pinned-item" else "item")))
