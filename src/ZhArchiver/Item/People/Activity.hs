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
import Data.Aeson.TH 
import Data.Aeson.Types
import Data.Bifunctor
import Data.Text (Text)
import qualified Data.Text as T
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
import ZhArchiver.Raw
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

deriveFromJSON defaultOptions {constructorTagModifier = drop 3} ''ActTarget
deriveToJSON defaultOptions {constructorTagModifier = camelTo2 '_' . drop 3} ''ActTarget

data Activity = Activity
  { actId :: Text,
    actTime :: Time,
    actTarget :: ActTarget
  }

deriveFromJSON defaultOptions {fieldLabelModifier = drop 3} ''Activity
deriveToJSON defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3} ''Activity

instance ShowId Activity where
  showType = const "activity"
  showId v =
    case actTarget v of
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

instance FromRaw Activity where
  parseRaw =
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
                              "answer" -> ActAnswer <$> parseRaw tar
                              "article" -> ActArticle <$> $(mkArticleParser False) tar
                              "column" -> ActColumn <$> parseRaw tar
                              "collection" -> ActCollection <$> parseRaw tar
                              "people" -> ActPeople <$> parseRaw tar
                              "pin" -> ActPin <$> parseRaw tar
                              "question" -> ActQuestion <$> parseRaw tar
                              _ -> pure (ActOther aid)
                        )
                        tar
                  )
          tim <- convertTime <$> o .: "created_time"
          return
            Activity
              { actId = aid,
                actTime = tim,
                actTarget = tar
              }
      )

instance ZhData Activity

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
    first (\t -> v {actTarget = t})
      <$> ( case actTarget v of
              ActAnswer a -> first ActAnswer <$> attachComment cli a
              ActArticle a -> first ActArticle <$> attachComment cli a
              ActCollection c -> first ActCollection <$> attachComment cli c
              ActPin p -> first ActPin <$> attachComment cli p
              ActQuestion q -> first ActQuestion <$> attachComment cli q
              o -> pure (o, emptyRm)
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
  fetchItemsRaw cli _ _ People {pUrlToken = PId uid} =
    do
      sp <- $(apiPath "web_moments" "activities") uid
      fmap Raw
        <$> reqPaging
          cli
          (httpsURI sp [])
