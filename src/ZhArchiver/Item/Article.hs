{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Article (ArtId (..), Article (..)) where

import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.Int (Int64)
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
import ZhArchiver.RawParser.TH
import ZhArchiver.Types

newtype ArtId = ArtId Int
  deriving newtype (Show, FromJSON, ToJSON)

data Article = Article
  { artId :: ArtId,
    artTitle :: Text,
    artImage, artTitleImage :: Maybe Image,
    artAuthor :: Author,
    artCreate, artUpdate :: Time,
    artVote :: Int64,
    artContent :: Content,
    artCommentCount :: Int,
    artComment :: [Comment],
    artRawData :: JSON.Value
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''Article

instance Item Article where
  type IId Article = ArtId
  type Signer Article = ()

  fetchRaw _ (ArtId i) =
    Raw . responseBody
      <$> req
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "articles" /: T.pack (show i))
        NoReqBody
        jsonResponse
        mempty

instance ZhData Article where
  parseRaw (Raw v) =
    $( rawParser
         'Article
         [ ('artId, foStock "id"),
           ('artTitle, foStock "title"),
           ('artImage, FoParse "image_url" poImageMaybe),
           ('artTitleImage, FoParse "title_image" poImageMaybe),
           ('artAuthor, FoParse "author" poAuthor),
           ('artCreate, FoParse "created" poTime),
           ('artUpdate, FoParse "updated" poTime),
           ('artVote, foStock "voteup_content"),
           ('artContent, FoParse "content" poContent),
           ('artCommentCount, foStock "comment_count"),
           ('artComment, FoConst (listE [])),
           ('artRawData, FoRaw)
         ]
     )
      v

instance Commentable Article where
  commentCount = artCommentCount
  attachComment a = (\c -> a {artComment = c}) <$> fetchComment StArticle (T.pack (show (artId a)))

deriveHasImage ''Article ['artAuthor, 'artImage, 'artTitleImage, 'artContent, 'artComment]