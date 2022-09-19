{-# LANGUAGE MultiParamTypeClasses #-}

module ZhArchiver.REPL.Util
  ( --
    Config (..),
    setupCfg,
    runCfg,
    --
    pullItemI,
    pullItemCI,
    pullChildI,
    pullChildCI,
    pullQuestionAns,
  )
where

import Control.Monad.Reader
import Data.Typeable
import Network.HTTP.Req
import System.FilePath
import ZhArchiver.Comment
import ZhArchiver.Image
import ZhArchiver.Item
import ZhArchiver.Item.Answer
import ZhArchiver.Item.Question
import ZhArchiver.Progress
import ZhArchiver.Raw
import ZhArchiver.Request.Zse96V3

data Config = Config
  { cfgCli :: Cli,
    cfgHome, cfgImgStore :: FilePath
  }

setupCfg :: Int -> FilePath -> FilePath -> Config
setupCfg wid hom img =
  Config
    { cfgCli = defaultCli {cliMaxWidth = wid},
      cfgHome = hom,
      cfgImgStore = img
    }

type WithCfg a = ReaderT Config (ImgFetcher Req) a

runCfg :: Config -> WithCfg a -> IO a
runCfg cfg v = do
  (a, f) <- runReq defaultHttpConfig $ runImgFetcher $ runReaderT v cfg
  saveImgFiles (cfgImgStore cfg) f
  putNewline
  return a

pullItemWith ::
  forall a.
  (Item a, HasImage a) =>
  (Cli -> a -> ImgFetcher Req a) ->
  Proxy a ->
  IId a ->
  Signer a ->
  WithCfg a
pullItemWith com _ aid sign = do
  cli <- asks (pushHeader (showType @a Proxy) . cfgCli)
  home <- asks cfgHome
  r <- lift (fetchItem @a sign aid >>= com cli >>= fetchImage cli)
  liftIO $ saveZhData (home </> showType @a Proxy) r
  return r

pullItemI ::
  forall a.
  (Item a, HasImage a) =>
  Proxy a ->
  IId a ->
  Signer a ->
  WithCfg (WithRaw a)
pullItemI _ = pullItemWith (const pure) Proxy

pullItemCI ::
  forall a.
  (Item a, Commentable a, HasImage a) =>
  Proxy a ->
  IId a ->
  Signer a ->
  WithCfg (WithRaw a)
pullItemCI _ = pullItemWith getComment Proxy

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
    cli <- asks (pushHeader (showType @i Proxy) . cfgCli)
    rs <- lift (fetchChildItems @a @i cli opt sig f >>= com cli >>= fetchImage cli)
    liftIO $ storeChildItems @a @i Proxy (home </> showType @a Proxy </> showId f) opt rs
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

pullQuestionAns :: ZseState -> QId -> WithCfg ()
pullQuestionAns sign qid = do
  q <- pullItemCI @Question Proxy qid sign
  void (pullChildCI @Question @Answer Proxy () (wrVal q) sign)