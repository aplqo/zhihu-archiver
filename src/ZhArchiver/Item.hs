{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ZhArchiver.Item
  ( ZhData (..),
    saveZhData,
    Item (..),
    IId (..),
    fetchItem,
    fetchItems,
    ItemContainer (..),
    fetchChildItems,
    storeChildItems,
  )
where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson.Types
import Data.Foldable (traverse_)
import Data.Type.Equality (type (~~))
import Data.Typeable
import Network.HTTP.Req
import System.Directory
import System.FilePath
import ZhArchiver.Progress
import ZhArchiver.Raw
import ZhArchiver.Util

{- Item store: {id}/data files
    store*: path is {id}/
    save*: path is parent dir of {id}
-}

class (FromRaw d, ShowId d) => ZhData d where
  storeData :: FilePath -> d -> IO ()
  default storeData :: (ToJSON d) => FilePath -> d -> IO ()
  storeData p = encodeFilePretty (p </> "info.json")

  loadData :: FilePath -> Decoder d
  default loadData :: (FromJSON d) => FilePath -> Decoder d
  loadData p = decodeFile (p </> "info.json")

instance (ZhData a) => ZhData (WithRaw a) where
  storeData p r@(WithRaw {wrVal = v}) =
    storeData p v >> storeRaw p r
  loadData p = loadData p >>= attachRaw p

saveZhData :: (ZhData a) => FilePath -> a -> IO ()
saveZhData p v =
  let dir = p </> showId v
   in createDirectoryIfMissing True dir >> storeData dir v

class (ZhData a) => Item a where
  data IId a
  type Signer a
  fetchRaw :: (MonadHttp m, MonadThrow m) => Signer a -> IId a -> m (RawData a)

instance (Item a) => Item (WithRaw a) where
  newtype IId (WithRaw a) = Wr {unWr :: IId a}
  type Signer (WithRaw a) = Signer a
  fetchRaw s i = Raw . unRaw <$> fetchRaw @a s (unWr i)

fetchItem :: (Item a, MonadHttp m, MonadThrow m) => Signer a -> IId a -> m a
fetchItem s i = runRawParser parseRaw <$> fetchRaw s i

fetchItems :: forall a m. (Item a, Show (IId a), MonadHttp m, MonadThrow m) => Cli -> Signer a -> [IId a] -> m [a]
fetchItems cli s is =
  let tot = length is
   in traverse
        ( \(i, idx) ->
            liftIO (showProgress cli ("Fetching " ++ show i ++ " " ++ show idx ++ "/" ++ show tot))
              >> (runRawParser parseRaw <$> fetchRaw s i)
        )
        (zip is [(1 :: Int) ..])
        <* liftIO (endProgress cli)

class (Item a, ZhData i) => ItemContainer a i where
  type ICOpt a i
  type ICSigner a i
  fetchItemsRaw :: (MonadHttp m, MonadThrow m) => Cli -> ICOpt a i -> ICSigner a i -> a -> m [RawData i]

  parseRawChild :: a -> RawData i -> Parser i
  parseRawChild _ = parseRaw . unRaw

  childStorePath :: Proxy a -> Proxy i -> FilePath -> ICOpt a i -> FilePath
  default childStorePath :: (ICOpt a i ~~ ()) => Proxy a -> Proxy i -> FilePath -> ICOpt a i -> FilePath
  childStorePath _ _ p () = p </> showType @i Proxy

instance (ItemContainer a i) => ItemContainer a (WithRaw i) where
  type ICOpt a (WithRaw i) = ICOpt a i
  type ICSigner a (WithRaw i) = ICSigner a i
  fetchItemsRaw cli opt sig v = fmap (Raw . unRaw) <$> fetchItemsRaw @a @i cli opt sig v
  parseRawChild f (Raw v) =
    (\r -> WithRaw {wrVal = r, wrRawData = Just (singletonRm "main" (RtLeaf v))})
      <$> parseRawChild @a @i f (Raw v)
  childStorePath _ _ = childStorePath @a @i Proxy Proxy

fetchChildItems ::
  forall a i m.
  (ItemContainer a i, MonadHttp m, MonadThrow m) =>
  Cli ->
  ICOpt a i ->
  ICSigner a i ->
  a ->
  m [i]
fetchChildItems cli opt sig v =
  fmap (runParser (parseRawChild v))
    <$> fetchItemsRaw cli opt sig v

storeChildItems :: forall a i. (ItemContainer a i) => Proxy a -> FilePath -> ICOpt a i -> [i] -> IO ()
storeChildItems _ p opt =
  traverse_ (saveZhData (childStorePath @a @i Proxy Proxy p opt))