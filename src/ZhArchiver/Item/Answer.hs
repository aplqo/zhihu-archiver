{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Answer (AId (..), Answer (..)) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Bifunctor
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH (listE)
import Network.HTTP.Req
import ZhArchiver.Author
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image.TH (deriveHasImage)
import ZhArchiver.Item
import {-# SOURCE #-} ZhArchiver.Item.Question (QId)
import ZhArchiver.Progress
import ZhArchiver.Raw
import ZhArchiver.RawParser.TH
import ZhArchiver.Request.Zse96V3
import ZhArchiver.Types

newtype AId = AId Int
  deriving newtype (Show, FromJSON, ToJSON)

data Answer = Answer
  { aId :: AId,
    aAuthor :: Maybe Author,
    aQuestion :: (QId, Text),
    aCreated, aUpdated :: Time,
    aVoteUp :: Int64,
    aContent :: Content,
    aCommentCount :: Int,
    aComment :: [Comment]
  }
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = tail} ''Answer

instance ShowId Answer where
  showType = const "answer"
  showId Answer {aId = AId i} = show i

instance FromRaw Answer where
  parseRaw =
    $( rawParser
         'Answer
         [ ('aId, foStock "id"),
           ('aAuthor, foFromRaw "author"),
           ('aQuestion, FoParse "question" (PoBind [|parseQuestion|])),
           ('aCreated, FoParse "created_time" poTime),
           ('aUpdated, FoParse "updated_time" poTime),
           ('aVoteUp, foStock "voteup_count"),
           ('aContent, FoParse "content" poContent),
           ('aCommentCount, foStock "comment_count"),
           ('aComment, FoConst (listE []))
         ]
     )
    where
      parseQuestion =
        withObject
          "question"
          ( \o -> do
              i <- o .: "id"
              t <- o .: "title"
              return (i, t)
          )

instance ZhData Answer

instance Item Answer where
  type IId Answer = AId
  type Signer Answer = ZseState

  fetchRaw zs aid =
    Raw . responseBody
      <$> reqCb
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "answers" /: T.pack (show aid))
        NoReqBody
        jsonResponse
        ("include" =: ("content;comment_count;voteup_count" :: Text))
        (zse96 zs)

instance Commentable Answer where
  hasComment a = aCommentCount a /= 0
  attachComment cli a =
    bimap (\c -> a {aComment = c}) (singletonRm "comment" . packLeaf)
      <$> fetchComment
        (pushHeader "comments" cli)
        StAnswer
        (T.pack (show (aId a)))

deriveHasImage
  ''Answer
  [ ('aAuthor, "author"),
    ('aContent, "content"),
    ('aComment, "comment")
  ]