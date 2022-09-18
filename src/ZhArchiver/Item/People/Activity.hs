{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item.People.Activity
  ( ActTarget (..),
    Activity (..),
  )
where

import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath
import ZhArchiver.Comment
import ZhArchiver.Image
import ZhArchiver.Item
import ZhArchiver.Item.Answer
import ZhArchiver.Item.Article
import ZhArchiver.Item.Article.Parser
import ZhArchiver.Item.Collection
import ZhArchiver.Item.Column
import ZhArchiver.Item.People
import ZhArchiver.Item.Pin
import ZhArchiver.Item.Question
import ZhArchiver.Progress
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri
import ZhArchiver.Types

data ActTarget
  = ActAnswer Answer
  | ActArticle Article
  | ActColumn Column
  | ActCollection Collection
  | ActPeople People
  | ActPin Pin
  | ActQuestion Question
  | ActOther Text
  deriving (Show)

deriveJSON defaultOptions {constructorTagModifier = drop 3} ''ActTarget

data Activity = Activity
  { actId :: Text,
    actTime :: Time,
    actTarget :: ActTarget,
    actRawData :: JSON.Value
  }

deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''Activity

instance ShowId Activity where
  showType = const "activity"
  showId = T.unpack . actId

instance ZhData Activity where
  parseRaw (Raw v) =
    withObject
      "Activity"
      ( \o -> do
          aid <- o .: "id"
          tar <-
            o .: "target"
              >>= ( \tar ->
                      withObject
                        "target"
                        ( \t ->
                            ((t .: "type") :: Parser Text) >>= \case
                              "answer" -> (\a -> ActAnswer a {aRawData = JSON.Null}) <$> parseRaw (Raw tar)
                              "article" -> (\a -> ActArticle a {artRawData = JSON.Null}) <$> $(mkArticleParser False) tar
                              "column" -> (\c -> ActColumn c {coRawData = JSON.Null}) <$> parseRaw (Raw tar)
                              "collection" -> (\c -> ActCollection c {colRawData = JSON.Null}) <$> parseRaw (Raw tar)
                              "people" -> (\p -> ActPeople p {pRawData = JSON.Null}) <$> parseRaw (Raw tar)
                              "pin" -> (\p -> ActPin p {pinRawData = JSON.Null}) <$> parseRaw (Raw tar)
                              "question" -> (\q -> ActQuestion q {qRawData = JSON.Null}) <$> parseRaw (Raw tar)
                              _ -> pure (ActOther aid)
                        )
                        tar
                  )
          tim <- convertTime <$> o .: "created_time"
          return
            Activity
              { actId = aid,
                actTime = tim,
                actTarget = tar,
                actRawData = v
              }
      )
      v
  saveData pat v =
    withDirectory
      ( pat
          </> ( case actTarget v of
                  ActAnswer a ->
                    "answer_" ++ showId a
                  ActArticle a ->
                    "article_" ++ showId a
                  ActColumn c ->
                    "column_" ++ showId c
                  ActCollection c ->
                    "collection_" ++ showId c
                  ActPeople p ->
                    "people_" ++ showId p
                  ActPin p ->
                    "pin_" ++ showId p
                  ActQuestion q ->
                    "question_" ++ showId q
                  ActOther t ->
                    "unknown_" ++ T.unpack t
              )
      )
      $ encodeFilePretty "info.json" v

instance Commentable Activity where
  hasComment v =
    case actTarget v of
      ActAnswer a -> hasComment a
      ActArticle a -> hasComment a
      ActCollection c -> hasComment c
      ActPin p -> hasComment p
      ActQuestion q -> hasComment q
      _ -> False
  attachComment cli v =
    (\t -> v {actTarget = t})
      <$> ( case actTarget v of
              ActAnswer a -> ActAnswer <$> attachComment cli a
              ActArticle a -> ActArticle <$> attachComment cli a
              ActCollection c -> ActCollection <$> attachComment cli c
              ActPin p -> ActPin <$> attachComment cli p
              ActQuestion q -> ActQuestion <$> attachComment cli q
              o -> pure o
          )

instance HasImage Activity where
  fetchImage cli v =
    (\t -> v {actTarget = t})
      <$> ( case actTarget v of
              ActAnswer a -> ActAnswer <$> fetchImage cli a
              ActArticle a -> ActArticle <$> fetchImage cli a
              ActColumn c -> ActColumn <$> fetchImage cli c
              ActCollection c -> ActCollection <$> fetchImage cli c
              ActPeople p -> ActPeople <$> fetchImage cli p
              ActPin p -> ActPin <$> fetchImage cli p
              ActQuestion q -> ActQuestion <$> fetchImage cli q
              o@(ActOther _) -> pure o
          )

instance ItemContainer People Activity where
  type ICOpt People Activity = ()
  type ICSigner People Activity = ()
  fetchItemsRaw cli _ _ People {pUrlToken = uid} =
    do
      sp <- $(apiPath "web_moments" "activities") uid
      fmap Raw
        <$> reqPaging
          (pushHeader "activity" cli)
          (httpsURI sp [])
  saveItems p _ s =
    traverse_ (saveData (p </> showId s </> "activity"))
