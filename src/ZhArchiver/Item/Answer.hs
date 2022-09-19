{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Answer (IId (..), Answer (..)) where

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
import {-# SOURCE #-} ZhArchiver.Item.Question
import ZhArchiver.Progress
import ZhArchiver.Raw
import ZhArchiver.RawParser.TH
import ZhArchiver.Request.Zse96V3
import ZhArchiver.Types

data Answer = Answer
  { aId :: IId Answer,
    aAuthor :: Maybe Author,
    aQuestion :: (IId Question, Text),
    aCreated, aUpdated :: Time,
    aVoteUp :: Int64,
    aContent :: Content,
    aCommentCount :: Int,
    aComment :: [Comment]
  }

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
  newtype IId Answer = AId Int64
    deriving (Show)
    deriving newtype (FromJSON, ToJSON)
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

deriving instance (Show Answer)

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