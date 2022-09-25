{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
    pullColumn,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable
import Data.Typeable
import Network.HTTP.Req
import System.FilePath
import ZhArchiver.Comment
import ZhArchiver.Image
import ZhArchiver.Item
import ZhArchiver.Item.AnsOrArt
import ZhArchiver.Item.Answer
import ZhArchiver.Item.Column
import ZhArchiver.Item.Question
import ZhArchiver.Progress
import ZhArchiver.Raw
import ZhArchiver.Request.Zse96V3

{-
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
-}

data Config = Config
  { cfgCli :: Cli,
    cfgHome, cfgImgMap :: FilePath
  }

setupCfg :: Int -> FilePath -> FilePath -> Config
setupCfg wid hom img =
  Config
    { cfgCli = defaultCli {cliMaxWidth = wid},
      cfgHome = hom,
      cfgImgMap = img
    }

type WithCfg a = ReaderT Config (ImgSaver Req) a

runCfg :: Config -> WithCfg a -> IO a
runCfg cfg v = do
  let is = cfgImgMap cfg
  l1 <-
    liftIO
      ( ( \case
            Left l -> error l
            Right r -> r
        )
          <$> runExceptT (loadLinks is)
      )
  (a, f) <- runReq defaultHttpConfig $ runImgSaver (runReaderT v cfg) l1
  storeLinks is f
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
  (r, m) <- lift (lift (runImgFetcher (fetchItem @a sign aid >>= com cli >>= fetchImage cli)))
  let dest = home </> showType @a Proxy
  liftIO $ saveZhData False dest r
  lift (saveImgFiles (dest </> showId r </> "image") m)
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

{-
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
-}

childCli :: forall a i. (ShowId a, ShowId i) => a -> Proxy i -> WithCfg Cli
childCli a p = asks (pushHeader (showType p) . pushHeader (showValId a) . cfgCli)

pullChildWith ::
  forall a i.
  (ItemContainer a i, HasImage i) =>
  (Cli -> [i] -> Req [i]) ->
  Proxy i ->
  ICOpt a i ->
  a ->
  ICSigner a i ->
  WithCfg [i]
pullChildWith com _ opt f sig =
  do
    home <- asks cfgHome
    cli <- childCli f (Proxy @i)
    ret <- lift (runReq defaultHttpConfig (fetchChildItems @a @i cli opt sig f >>= com cli >>= traverseP cli (\c i -> runImgFetcher (fetchImage c i))))
    let dest = home </> showType @a Proxy </> showId f
        items = fmap fst ret
        storePath = childStorePath @a @i Proxy Proxy dest opt
    liftIO $ storeChildItems @a @i Proxy False dest opt items
    lift $
      traverse_
        (\(item, img) -> saveImgFiles (storePath </> showId item </> "image") img)
        ret
    return items

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

{-
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
-}

pullQuestionAns :: ZseState -> IId (WithRaw Question) -> WithCfg ()
pullQuestionAns sign qid = do
  q <- pullItemCI @Question qid sign
  void (pullChildCI @Question @Answer Proxy () (wrVal q) sign)

pullColumn :: IId (WithRaw Column) -> WithCfg ()
pullColumn iid = do
  c <- wrVal <$> pullItemI @Column iid ()
  void (pullChildCI @Column @AnsOrArt Proxy True c ())
  void (pullChildCI @Column @AnsOrArt Proxy False c ())