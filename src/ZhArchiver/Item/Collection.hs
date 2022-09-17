{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Collection (Collection (..)) where

import Control.Applicative
import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH (listE)
import Network.HTTP.Req
import ZhArchiver.Author
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image.TH (deriveHasImage)
import ZhArchiver.Item
import ZhArchiver.Item.AnsOrArt
import ZhArchiver.Progress
import ZhArchiver.RawParser.TH
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri hiding (https)
import ZhArchiver.Types

data Collection = Collection
  { colId :: Int,
    colTitle :: Text,
    colDescription :: Maybe Content,
    colCreated, colUpdated :: Time,
    colCreator :: Author,
    colCommentCount :: Int,
    colComment :: [Comment],
    colRawData :: JSON.Value
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''Collection

deriveHasImage
  ''Collection
  [ ('colDescription, "description"),
    ('colCreator, "creator"),
    ('colComment, "comment")
  ]

instance ShowId Collection where
  showType = const "collection"
  showId Collection {colId = i} = show i

instance ZhData Collection where
  parseRaw (Raw v) =
    parser v
      <|> withObject
        "Collection Response"
        ( \o ->
            o .: "collection"
              >>= fmap (\c -> c {colRawData = v}) . parser
        )
        v
    where
      parser =
        $( rawParser
             'Collection
             [ ('colId, foStock "id"),
               ('colTitle, foStock "title"),
               ('colDescription, FoParse "description" poContentMaybe),
               ('colCreated, FoParse "created_time" poTime),
               ('colUpdated, FoParse "updated_time" poTime),
               ('colCreator, FoParse "creator" poAuthor),
               ('colCommentCount, foStock "comment_count"),
               ('colComment, FoConst (listE [])),
               ('colRawData, FoRaw)
             ]
         )

instance Item Collection where
  type IId Collection = Int
  type Signer Collection = ()
  fetchRaw _ cid =
    Raw . responseBody
      <$> req
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "collections" /: T.pack (show cid))
        NoReqBody
        jsonResponse
        mempty

instance Commentable Collection where
  hasComment a = colCommentCount a /= 0
  attachComment cli a =
    (\c -> a {colComment = c})
      <$> fetchComment (pushHeader "comment" cli) StCollection (T.pack (show (colId a)))

instance ItemContainer Collection AnsOrArt where
  type ICOpt Collection AnsOrArt = ()
  type ICSigner Collection AnsOrArt = ()
  fetchItemsRaw cli _ _ Collection {colId = cid} = do
    sp <- $(apiPath "collections" "items") (T.pack (show cid))
    fmap Raw <$> reqPaging (pushHeader "item" cli) (httpsURI sp [])