{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.Question (IId (..), Question (..)) where

import Data.Aeson hiding (Value)
import Data.Aeson.TH
import Data.Bifunctor
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Network.HTTP.Req
import Text.URI
import Text.URI.QQ
import ZhArchiver.Author
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image.TH
import ZhArchiver.Item
import ZhArchiver.Item.Answer
import ZhArchiver.Progress
import ZhArchiver.Raw
import ZhArchiver.Raw.Parser.TH
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri (apiPath, httpsURI)
import ZhArchiver.Request.Zse96V3
import ZhArchiver.Types

data Question = Question
  { qId :: IId Question,
    qTitle :: Text,
    qAuthor :: Maybe Author,
    qCreated :: Time,
    qUpdated :: Maybe Time,
    qContent :: Maybe Content,
    qCommentCount :: Int,
    qComments :: [Comment]
  }

$( concat
     <$> sequence
       [ deriveFromJSON defaultOptions {fieldLabelModifier = tail} ''Question,
         deriveToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . tail} ''Question
       ]
 )

instance ShowId Question where
  showType = const "question"
  showId Question {qId = QId q} = show q

instance ShowName Question where
  showName = T.unpack . qTitle

instance Item Question where
  newtype IId Question = QId Int
    deriving newtype (Show, FromJSON, ToJSON)
  type Signer Question = ZseState
  fetchRaw zs (QId qid) =
    Raw . responseBody
      <$> reqCb
        GET
        (https "www.zhihu.com" /: "api" /: "v4" /: "questions" /: T.pack (show qid))
        NoReqBody
        jsonResponse
        ("include" =: ("author,description,is_anonymous;detail;comment_count;answer_count;excerpt" :: Text))
        (zse96 zs)

deriving instance (Show Question)

instance FromRaw Question where
  parseRaw =
    $( rawParser
         'Question
         [ ('qId, FoParse "id" PoStock),
           ('qTitle, foStock "title"),
           ('qAuthor, foFromRaw "author"),
           ('qCreated, FoParse "created" poTime),
           ('qUpdated, FoParseMaybe "updated_time" False poTime),
           ('qContent, FoParse "detail" poContentMaybe),
           ('qCommentCount, FoParse "comment_count" PoStock),
           ('qComments, FoConst (listE []))
         ]
     )

instance ZhData Question

instance Commentable Question where
  hasComment = (/= 0) . qCommentCount
  attachComment cli v =
    bimap (\c -> v {qComments = c}) (singletonRm "comment" . packLeaf)
      <$> fetchComment (pushHeader "comment" cli) StQuestion (T.pack (show (qId v)))

deriveHasImage ''Question [('qAuthor, "author"), ('qContent, "content"), ('qComments, "comments")]

instance ItemContainer Question Answer where
  type ICOpt Question Answer = ()
  type ICSigner Question Answer = ZseState
  fetchItemsRaw cli _ zs Question {qId = qid} =
    do
      p <- $(apiPath "questions" "answers") (T.pack (show qid))
      fmap Raw
        <$> reqPagingSign
          cli
          ( httpsURI
              p
              [QueryParam [queryKey|include|] [queryValue|data[*].is_normal,admin_closed_comment,reward_info,is_collapsed,annotation_action,annotation_detail,collapse_reason,is_sticky,collapsed_by,suggest_edit,comment_count,can_comment,content,editable_content,attachment,voteup_count,reshipment_settings,comment_permission,created_time,updated_time,review_info,relevant_info,question,excerpt,is_labeled,paid_info,paid_info_content,reaction_instruction,relationship.is_authorized,is_author,voting,is_thanked,is_nothelp,is_recognized;data[*].mark_infos[*].url;data[*].author.follower_count,vip_info,badge[*].topics;data[*].settings.table_of_content.enabled|]]
          )
          (zse96 zs)