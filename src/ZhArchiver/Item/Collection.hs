{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Collection (IId (..), Collection (..), ColItem (..)) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Bifunctor
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH (listE)
import Network.HTTP.Req
import ZhArchiver.Author
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH (deriveHasImage)
import ZhArchiver.Item
import ZhArchiver.Item.AnsOrArt
import ZhArchiver.Progress
import ZhArchiver.Raw
import ZhArchiver.Raw.Parser.TH
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri hiding (https)
import ZhArchiver.Types

data Collection = Collection
  { colId :: IId Collection,
    colTitle :: Text,
    colDescription :: Maybe Content,
    colCreated, colUpdated :: Time,
    colCreator :: Author,
    colCommentCount :: Int,
    colComment :: [Comment]
  }

deriveJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3} ''Collection

instance ShowId Collection where
  showType = const "collection"
  showId Collection {colId = i} = show i

instance ShowName Collection where
  showName = T.unpack . colTitle

instance FromRaw Collection where
  parseRaw =
    $( rawParser
         'Collection
         [ ('colId, foStock "id"),
           ('colTitle, foStock "title"),
           ('colDescription, FoParse "description" poContentMaybe),
           ('colCreated, FoParse "created_time" poTime),
           ('colUpdated, FoParse "updated_time" poTime),
           ('colCreator, foFromRaw "creator"),
           ('colCommentCount, foStock "comment_count"),
           ('colComment, FoConst (listE []))
         ]
     )

instance ZhData Collection

instance Item Collection where
  newtype IId Collection = ColId Int
    deriving newtype (Show, FromJSON, ToJSON)
  type Signer Collection = ()
  fetchRaw _ cid =
    Raw . responseBody
      <$> req
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "collections" /: T.pack (show cid))
        NoReqBody
        jsonResponse
        mempty

deriving instance (Show Collection)

instance Commentable Collection where
  hasComment a = colCommentCount a /= 0
  attachComment cli a =
    bimap (\c -> a {colComment = c}) (singletonRm "comment" . packLeaf)
      <$> fetchComment (pushHeader "comment" cli) StCollection (T.pack (show (colId a)))

deriveHasImage
  ''Collection
  [ ('colDescription, "description"),
    ('colCreator, "creator"),
    ('colComment, "comment")
  ]

-- | api response contains more information than collection/item
newtype ColItem = ColItem {colItBody :: AnsOrArt}
  deriving (Show)
  deriving newtype (FromJSON, ToJSON, ShowId, ShowName, HasContent)

instance FromRaw ColItem where
  parseRaw =
    $( rawParser
         'ColItem
         [ ('colItBody, foFromRaw "content")
         ]
     )

instance ZhData ColItem

instance Commentable ColItem where
  hasComment v = hasComment (colItBody v)
  attachComment cli v = first (\c -> v {colItBody = c}) <$> attachComment cli (colItBody v)

instance HasImage ColItem where
  fetchImage cli v = (\b -> v {colItBody = b}) <$> fetchImage cli (colItBody v)
  imageSet = imageSet . colItBody

instance ItemContainer Collection ColItem where
  type ICOpt Collection ColItem = ()
  type ICSigner Collection ColItem = ()
  fetchItemsRaw cli _ _ Collection {colId = cid} = do
    sp <- $(apiPath "collections" "items") (T.pack (show cid))
    fmap Raw <$> reqPaging cli (httpsURI sp [])