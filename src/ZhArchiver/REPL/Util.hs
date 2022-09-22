{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ZhArchiver.REPL.Util
  ( --
    Config (..),
    setupCfg,
    runCfg,
    --
    pullItemI,
    pullItemCI,
    pullItemCID,
    pullChildI,
    pullChildCI,
    pullChildCID,
    pullQuestionAns,
    pullColumn,
  )
where

import Control.Monad.Reader
import Data.Default
import Data.Maybe
import qualified Data.Text.IO as TIO
import Data.Typeable
import Network.HTTP.Req
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import qualified Text.Pandoc as P
import Text.Pandoc.Writers
import ZhArchiver.Comment
import ZhArchiver.Content
import ZhArchiver.Image
import ZhArchiver.Item
import ZhArchiver.Item.AnsOrArt
import ZhArchiver.Item.Answer
import ZhArchiver.Item.Column
import ZhArchiver.Item.Question
import ZhArchiver.Progress
import ZhArchiver.REPL.FilePath
import ZhArchiver.Raw
import ZhArchiver.Request.Zse96V3

saveContent :: (ShowId a, ShowName a, HasContent a) => FilePath -> FilePath -> Cli -> a -> IO ()
saveContent doc img cli a = do
  imgp <- fromJust <$> makeRelativeEx doc img
  con <-
    P.runIOorExplode $
      convertContent imgp a
        >>= maybe
          (pure Nothing)
          (fmap Just . writeMarkdown def)
  createDirectoryIfMissing True doc
  showMessage cli "Convert content to markdown"
  case con of
    Just cont -> TIO.writeFile (doc </> escapeSlash (showValName a) <.> "md") cont
    Nothing -> pure ()
  where
    escapeSlash [] = []
    escapeSlash (x : xs) =
      if x == '/'
        then '%' : '2' : 'F' : escapeSlash xs
        else x : escapeSlash xs

data Config = Config
  { cfgCli :: Cli,
    cfgHome, cfgImgStore, cfgDoc :: FilePath
  }

setupCfg :: Int -> FilePath -> FilePath -> FilePath -> Config
setupCfg wid doc hom img =
  Config
    { cfgCli = defaultCli {cliMaxWidth = wid},
      cfgHome = hom,
      cfgImgStore = img,
      cfgDoc = doc
    }

type WithCfg a = ReaderT Config (ImgFetcher Req) a

runCfg :: Config -> WithCfg a -> IO a
runCfg cfg v = do
  (a, f) <- runReq defaultHttpConfig $ runImgFetcher $ runReaderT v cfg
  saveImgFiles (cfgImgStore cfg) f
  putNewline
  return a

itemCli :: forall a. (ShowId a, Show (IId a)) => IId a -> WithCfg Cli
itemCli v = asks (pushHeader (showType @a Proxy ++ " " ++ show v) . cfgCli)

pullItemWith ::
  forall a.
  (Item a, HasImage a, Show (IId a)) =>
  (Cli -> a -> ImgFetcher Req a) ->
  IId a ->
  Signer a ->
  WithCfg a
pullItemWith com aid sign = do
  cli <- itemCli aid
  home <- asks cfgHome
  r <- lift (fetchItem @a sign aid >>= com cli >>= fetchImage cli)
  liftIO $ saveZhData False (home </> showType @a Proxy) r
  return r

pullItemI ::
  forall a.
  (Item a, HasImage a, Show (IId a)) =>
  IId (WithRaw a) ->
  Signer a ->
  WithCfg (WithRaw a)
pullItemI = pullItemWith (const pure)

pullItemCI ::
  (Item a, Commentable a, HasImage a, Show (IId a)) =>
  IId (WithRaw a) ->
  Signer a ->
  WithCfg (WithRaw a)
pullItemCI = pullItemWith getComment

pullItemCID ::
  forall a.
  (Item a, Commentable a, HasImage a, HasContent a, ShowName a, Show (IId a)) =>
  IId (WithRaw a) ->
  Signer a ->
  WithCfg (WithRaw a)
pullItemCID iid sig = do
  dat <- pullItemCI iid sig

  cli <- itemCli iid
  Config {cfgDoc = doc, cfgImgStore = img} <- ask
  liftIO (saveContent (doc </> showType @a Proxy) img cli dat)
  return dat

childCli :: forall a i. (ShowId a, ShowId i) => a -> Proxy i -> WithCfg Cli
childCli a p = asks (pushHeader (showType p) . pushHeader (showValId a) . cfgCli)

pullChildWith ::
  forall a i.
  (ItemContainer a i, HasImage i) =>
  (Cli -> [i] -> ImgFetcher Req [i]) ->
  Proxy i ->
  ICOpt a i ->
  a ->
  ICSigner a i ->
  WithCfg [i]
pullChildWith com _ opt f sig =
  do
    home <- asks cfgHome
    cli <- childCli f (Proxy @i)
    rs <- lift (fetchChildItems @a @i cli opt sig f >>= com cli >>= fetchImage cli)
    liftIO $ storeChildItems @a @i Proxy False (home </> showType @a Proxy </> showId f) opt rs
    return rs

pullChildI ::
  forall a i.
  (ItemContainer a i, HasImage i) =>
  Proxy i ->
  ICOpt a i ->
  a ->
  ICSigner a i ->
  WithCfg [WithRaw i]
pullChildI _ = pullChildWith (const pure) Proxy

pullChildCI ::
  forall a i.
  (ItemContainer a i, Commentable i, HasImage i) =>
  Proxy i ->
  ICOpt a i ->
  a ->
  ICSigner a i ->
  WithCfg [WithRaw i]
pullChildCI _ = pullChildWith @a @(WithRaw i) (`traverseP` getComment) Proxy

pullChildCID ::
  forall a i.
  (ItemContainer a i, ShowName a, Commentable i, HasImage i, HasContent i, ShowName i) =>
  Proxy i ->
  ICOpt a i ->
  a ->
  ICSigner a i ->
  WithCfg [WithRaw i]
pullChildCID _ opt f sig = do
  dats <- pullChildCI @a @i Proxy opt f sig

  docs <- asks (\cfg -> childStorePath @a @i Proxy Proxy (cfgDoc cfg </> showType @a Proxy </> showValName f) opt)
  img <- asks cfgImgStore
  cli <- childCli f (Proxy @i)
  void (liftIO (traverseP cli (saveContent docs img) dats))
  return dats

pullQuestionAns :: ZseState -> IId (WithRaw Question) -> WithCfg ()
pullQuestionAns sign qid = do
  q <- pullItemCI @Question qid sign
  void (pullChildCID @Question @Answer Proxy () (wrVal q) sign)

pullColumn :: IId (WithRaw Column) -> WithCfg ()
pullColumn iid = do
  c <- wrVal <$> pullItemI @Column iid ()
  void (pullChildCID @Column @AnsOrArt Proxy True c ())
  void (pullChildCID @Column @AnsOrArt Proxy False c ())