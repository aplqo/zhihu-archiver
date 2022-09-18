{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ZhArchiver.Comment
  ( SourceType (..),
    Comment (..),
    Commentable (..),
    fetchComment,
  )
where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Data.Aeson hiding (Value)
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import Text.URI.QQ (pathPiece)
import ZhArchiver.Author
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Image.TH
import ZhArchiver.Progress
import ZhArchiver.RawParser.Util
import ZhArchiver.Request.Paging
import ZhArchiver.Request.Uri hiding (https)
import ZhArchiver.Types

data SourceType
  = StArticle
  | StAnswer
  | StCollection
  | StQuestion
  | StPin

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
  deriving (Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''Comment

instance ShowId Comment where
  showType = const "comment"
  showId Comment {comId = c} = T.unpack c

deriveHasImage
  ''Comment
  [ ('comAuthor, "author"),
    ('comContent, "content"),
    ('comComment, "child_comment")
  ]

fetchCommentRaw :: MonadHttp m => Text -> m JSON.Value
fetchCommentRaw cid =
  responseBody
    <$> req
      GET
      (https "www.zhihu.com" /: "api" /: "v4" /: "comment_v5" /: "comment" /: cid)
      NoReqBody
      jsonResponse
      mempty

fetchRootCommentRaw :: (MonadHttp m, MonadThrow m) => Cli -> SourceType -> Text -> m [JSON.Value]
fetchRootCommentRaw cli st sid =
  do
    sp <-
      $(pathTemplate [F "api", F "v4", F "comment_v5", P, T, F "root_comment"])
        ( case st of
            StArticle -> [pathPiece|articles|]
            StAnswer -> [pathPiece|answers|]
            StCollection -> [pathPiece|collections|]
            StQuestion -> [pathPiece|questions|]
            StPin -> [pathPiece|pins|]
        )
        sid
    reqPaging cli (httpsURI sp [])

fetchChildCommentRaw :: (MonadHttp m, MonadThrow m) => Cli -> Text -> m [JSON.Value]
fetchChildCommentRaw cli sid =
  do
    sp <- $(pathTemplate [F "api", F "v4", F "comment_v5", F "comment", T, F "child_comment"]) sid
    reqPaging cli (httpsURI sp [])

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
                author <- o .: "author" >>= parseAuthorMaybe
                created <- convertTime <$> (o .: "created_time")
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
                  unlessMaybe (== "0") <$> o .: "reply_comment_id"

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

fetchChildComment :: (MonadHttp m, MonadThrow m) => Cli -> Text -> m [(Comment, Maybe Text)]
fetchChildComment cli cid = do
  raw <- fmap parse <$> fetchChildCommentRaw (pushHeader "child_comment" cli) cid
  liftIO $ showProgress cli "Getting missing comment"
  del <- complete (HS.fromList (cid : fmap (comId . fst) raw)) (parents raw) <* liftIO (endProgress cli)
  return (del ++ raw)
  where
    parse v =
      let (c, r, _) = parseRawComment v
       in (c, r)

    parents :: [(Comment, Maybe Text)] -> [Text]
    parents = foldr (\(_, i) c -> maybe c (: c) i) []

    comCli = pushHeader "missing_comment" cli

    complete has expect =
      let missing = filter (\i -> not (HS.member i has)) expect
       in if null missing
            then return []
            else do
              items <-
                traverse
                  ( \i ->
                      liftIO (showProgress comCli ("Getting comment " ++ T.unpack i))
                        *> (fmap parse . fetchCommentRaw) i
                  )
                  missing
              par <- complete (HS.union has (HS.fromList missing)) (parents items)
              return (items ++ par)

-- | sort comments to provide better git diff
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
   in buildChild childMap <$> sortOn comId roots
  where
    buildChild :: HM.HashMap Text [Comment] -> Comment -> Comment
    buildChild cm i =
      i
        { comComment = sortOn comId (buildChild cm <$> HM.findWithDefault [] (comId i) cm)
        }

class Commentable a where
  hasComment :: a -> Bool
  attachComment :: (MonadHttp m, MonadThrow m) => Cli -> a -> m a

instance (Commentable a, ShowId a) => Commentable [a] where
  hasComment = any hasComment
  attachComment cli v =
    let num = length v
     in traverse
          ( \(i, idx) ->
              if hasComment i
                then attachComment (appendHeader (" " ++ showId i ++ " " ++ show idx ++ "/" ++ show num) cli) i
                else pure i
          )
          (zip v [(1 :: Int) ..])

instance (Commentable a) => Commentable (Maybe a) where
  hasComment = maybe False hasComment
  attachComment cli = traverse (attachComment cli)

fetchComment :: (MonadHttp m, MonadThrow m) => Cli -> SourceType -> Text -> m [Comment]
fetchComment cli typ sid =
  buildCommentTree . concat
    <$> ( fetchRootCommentRaw (pushHeader "root_comment" cli) typ sid
            >>= traverse
              ( \i ->
                  let (c, f, cnt) = parseRawComment i
                   in if cnt == 0
                        then pure [(c, f)]
                        else
                          ((c, f) :)
                            <$> fetchChildComment
                              (pushHeader (showValId c) cli)
                              (comId c)
              )
        )
