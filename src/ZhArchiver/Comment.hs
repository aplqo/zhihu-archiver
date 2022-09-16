{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Comment
  ( SourceType (..),
    Comment (..),
    fetchComment,
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson hiding (Value)
import qualified Data.Aeson as JSON
import Data.Aeson.Types hiding (parse)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req
import Text.URI (mkPathPiece)
import Text.URI.QQ (pathPiece)
import ZhArchiver.Author
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri hiding (https)
import ZhArchiver.Types

data SourceType
  = StArticle
  | StAnswer
  | StCollection
  | StQuestion

data Comment = Comment
  { comId :: Text,
    comAuthor :: Maybe Author,
    comIsAuthor :: Bool,
    comCreated :: Time,
    comContent :: Maybe Content,
    comLike, comDislike :: Int,
    comComment :: [Comment],
    comRawData :: JSON.Value
  }
  deriving (Generic, Show)

instance FromJSON Comment where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

instance ToJSON Comment where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}

fetchCommentRaw :: MonadHttp m => Text -> m JSON.Value
fetchCommentRaw cid =
  responseBody
    <$> req
      GET
      (https "www.zhihu.com" /: "api" /: "v4" /: "comment_v5" /: "comment" /: cid)
      NoReqBody
      jsonResponse
      mempty

fetchRootCommentRaw :: (MonadHttp m, MonadThrow m) => SourceType -> Text -> m [JSON.Value]
fetchRootCommentRaw st sid =
  do
    sp <-
      $(pathTemplate [F "api", F "v4", F "comment_v5", P, T, F "root_comment"])
        ( case st of
            StArticle -> [pathPiece|articles|]
            StAnswer -> [pathPiece|answers|]
            StCollection -> [pathPiece|collections|]
            StQuestion -> [pathPiece|questions|]
        )
        sid
    reqPaging
      (httpsURI sp [])

fetchChildCommentRaw :: (MonadHttp m, MonadThrow m) => Text -> m [JSON.Value]
fetchChildCommentRaw sid =
  do
    sp <- $(pathTemplate [F "api", F "v4", F "comment_v5", F "comment", T, F "child_comment"]) sid
    reqPaging
      (httpsURI sp [])

-- | returns (body, reply, child)
parseRawComment :: JSON.Value -> (Comment, Maybe Text, Int)
parseRawComment =
  fromJust
    . parseMaybe
      ( \v ->
          withObject
            "comment body"
            ( \o -> do
                cid <- o .: "id"
                author <- o .: "author" >>= parseAuthor
                created <- o .: "created_time" >>= parseTime
                cont <-
                  o .: "is_delete" >>= \del ->
                    if del
                      then return Nothing
                      else
                        (\c -> Just (Content {contHtml = c, contImages = emptyImgMap}))
                          <$> o .: "content"
                isAuthor <- o .: "is_author"
                liked <- o .: "like_count"
                disliked <- o .: "dislike_count"

                reply <-
                  (\rId -> if rId == "0" then Nothing else Just rId)
                    <$> o .: "reply_comment_id"

                childC <- o .: "child_comment_count"

                return
                  ( Comment
                      { comId = cid,
                        comAuthor = author,
                        comIsAuthor = isAuthor,
                        comCreated = created,
                        comContent = cont,
                        comLike = liked,
                        comDislike = disliked,
                        comComment = [],
                        comRawData = v
                      },
                    reply,
                    childC
                  )
            )
            v
      )

fetchChildComment :: (MonadHttp m, MonadThrow m) => Text -> m [(Comment, Maybe Text)]
fetchChildComment cid = do
  raw <- fmap parse <$> fetchChildCommentRaw cid
  del <- complete (HS.fromList (cid : fmap (comId . fst) raw)) (parents raw)
  return (del ++ raw)
  where
    parse v =
      let (c, r, _) = parseRawComment v
       in (c, r)

    parents :: [(Comment, Maybe Text)] -> [Text]
    parents = foldr (\(_, i) c -> maybe c (: c) i) []

    complete has expect =
      let missing = filter (\i -> not (HS.member i has)) expect
       in if null missing
            then return []
            else do
              items <- traverse (fmap parse . fetchCommentRaw) missing
              par <- complete (HS.union has (HS.fromList missing)) (parents items)
              return (items ++ par)

buildCommentTree :: [(Comment, Maybe Text)] -> [Comment]
buildCommentTree cs =
  let (roots, childMap) =
        foldr
          ( \i (rl, cm) ->
              case snd i of
                Just fa -> (rl, HM.insertWith (++) fa [fst i] cm)
                Nothing -> (fst i : rl, cm)
          )
          ([], HM.empty)
          cs
   in buildChild childMap <$> roots
  where
    buildChild :: HM.HashMap Text [Comment] -> Comment -> Comment
    buildChild cm i =
      i
        { comComment = buildChild cm <$> HM.findWithDefault [] (comId i) cm
        }

fetchComment :: (MonadHttp m, MonadThrow m) => SourceType -> Text -> m [Comment]
fetchComment typ sid =
  buildCommentTree . concat
    <$> ( fetchRootCommentRaw typ sid
            >>= traverse
              ( \i ->
                  let (c, f, cnt) = parseRawComment i
                   in if cnt == 0
                        then pure [(c, f)]
                        else ((c, f) :) <$> fetchChildComment (comId c)
              )
        )

deriveHasImage ''Comment ['comAuthor, 'comContent, 'comComment]