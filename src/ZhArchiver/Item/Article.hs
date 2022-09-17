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
import System.FilePath
import ZhArchiver.Author
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH (deriveHasImage)
import ZhArchiver.Item
import ZhArchiver.Progress
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

instance ShowId Article where
  showType = const "article"
  showId Article {artId = ArtId a} = show a

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
           ('artTitleImage, FoParseMaybe "title_image" True poImageMaybe),
           ('artAuthor, FoParse "author" poAuthor),
           ('artCreate, FoParse "created" poTime),
           ('artUpdate, FoParse "updated" poTime),
           ('artVote, foStock "voteup_count"),
           ('artContent, FoParse "content" poContent),
           ('artCommentCount, foStock "comment_count"),
           ('artComment, FoConst (listE [])),
           ('artRawData, FoRaw)
         ]
     )
      v
  saveData p a =
    withDirectory (p </> showId a) $
      encodeFilePretty "info.json" a

instance Commentable Article where
  hasComment a = artCommentCount a /= 0
  attachComment cli a =
    (\c -> a {artComment = c})
      <$> fetchComment
        (pushHeader "comments" cli)
        StArticle
        (T.pack (show (artId a)))

deriveHasImage
  ''Article
  [ ('artAuthor, "author"),
    ('artImage, "image"),
    ('artTitleImage, "title_image"),
    ('artContent, "content"),
    ('artComment, "comment")
  ]