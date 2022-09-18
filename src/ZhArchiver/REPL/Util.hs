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
import ZhArchiver.Request.Zse96V3

data Config = Config
  { cli :: Cli,
    home, imgStore :: FilePath
  }

setupCfg :: Int -> FilePath -> FilePath -> Config
setupCfg wid hom img =
  Config
    { cli = defaultCli {cliMaxWidth = wid},
      home = hom,
      imgStore = img
    }

type WithCfg a = ReaderT Config (ImgFetcher Req) a

runCfg :: Config -> WithCfg a -> IO a
runCfg cfg v = do
  (a, f) <- runReq defaultHttpConfig $ runImgFetcher $ runReaderT v cfg
  saveImgFiles (imgStore cfg) f
  putNewline
  return a

pullItemWith ::
  forall a.
  (Item a, HasImage a, ShowId a) =>
  (Cli -> a -> ImgFetcher Req a) ->
  Proxy a ->
  IId a ->
  Signer a ->
  WithCfg a
pullItemWith com _ aid sign = do
  cli <- asks (pushHeader (showType @a Proxy) . cli)
  home <- asks home
  r <- lift (fetchItem @a sign aid >>= com cli >>= fetchImage cli)
  liftIO $ saveData (home </> showType @a Proxy) r
  return r

pullItemI ::
  forall a.
  (Item a, HasImage a, ShowId a) =>
  Proxy a ->
  IId a ->
  Signer a ->
  WithCfg a
pullItemI = pullItemWith (const pure)

pullItemCI ::
  forall a.
  (Item a, Commentable a, HasImage a, ShowId a) =>
  Proxy a ->
  IId a ->
  Signer a ->
  WithCfg a
pullItemCI = pullItemWith attachComment

pullChildWith ::
  forall a i.
  (ItemContainer a i, HasImage i, ShowId a, ShowId i) =>
  (Cli -> [i] -> ImgFetcher Req [i]) ->
  Proxy i ->
  ICOpt a i ->
  a ->
  ICSigner a i ->
  WithCfg [i]
pullChildWith com _ opt f sig =
  do
    home <- asks home
    cli <- asks (pushHeader (showType @i Proxy) . cli)
    rs <- lift (fetchChildItems @a @i cli opt sig f >>= com cli >>= fetchImage cli)
    liftIO $ saveItems (home </> showType @a Proxy) opt f rs
    return rs

pullChildI ::
  forall a i.
  (ItemContainer a i, HasImage i, ShowId a, ShowId i) =>
  Proxy i ->
  ICOpt a i ->
  a ->
  ICSigner a i ->
  WithCfg [i]
pullChildI = pullChildWith (const pure)

pullChildCI ::
  forall a i.
  (ItemContainer a i, Commentable i, HasImage i, ShowId a, ShowId i) =>
  Proxy i ->
  ICOpt a i ->
  a ->
  ICSigner a i ->
  WithCfg [i]
pullChildCI = pullChildWith attachComment

pullQuestionAns :: ZseState -> QId -> WithCfg ()
pullQuestionAns sign qid = do
  q <- pullItemCI @Question Proxy qid sign
  void (pullChildCI @Question @Answer Proxy () q sign)