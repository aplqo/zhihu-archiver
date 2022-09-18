{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ZhArchiver.Item
  ( RawData (..),
    encodeFilePretty,
    withDirectory,
    ZhData (..),
    runParser,
    Item (..),
    fetchItem,
    fetchItems,
    ItemContainer (..),
    fetchChildItems,
  )
where

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import Data.Type.Equality (type (~~))
import Data.Typeable
import Network.HTTP.Req
import System.Directory
import System.FilePath
import ZhArchiver.Progress

newtype RawData a = Raw {unRaw :: JSON.Value}
  deriving (Show)

encodeFilePretty :: (ToJSON a) => FilePath -> a -> IO ()
encodeFilePretty p a =
  LBS.writeFile
    p
    ( encodePretty' defConfig {confIndent = Spaces 2} a
    )

withDirectory :: FilePath -> IO a -> IO a
withDirectory p a =
  createDirectoryIfMissing True p
    >> withCurrentDirectory p a

class ZhData d where
  parseRaw :: RawData d -> Parser d
  saveData :: FilePath -> d -> IO ()
  default saveData :: (ToJSON d, ShowId d) => FilePath -> d -> IO ()
  saveData p v =
    withDirectory (p </> showId v) $
      encodeFilePretty "info.json" v

  loadData :: FilePath -> IO (Either String d)
  default loadData :: (FromJSON d) => FilePath -> IO (Either String d)
  loadData p = JSON.eitherDecodeFileStrict (p </> "info.json")

runParser :: (a -> Parser b) -> a -> b
runParser p v =
  case parse p v of
    Success r -> r
    Error e -> error ("Parse error:" ++ e)

class (ZhData a) => Item a where
  type IId a
  type Signer a
  fetchRaw :: (MonadHttp m, MonadThrow m) => Signer a -> IId a -> m (RawData a)

fetchItem :: (Item a, MonadHttp m, MonadThrow m) => Signer a -> IId a -> m a
fetchItem s i = runParser parseRaw <$> fetchRaw s i

fetchItems :: (Item a, Show (IId a), MonadHttp m, MonadThrow m) => Cli -> Signer a -> [IId a] -> m [a]
fetchItems cli s is =
  let tot = length is
   in traverse
        ( \(i, idx) ->
            liftIO (showProgress cli ("Fetching " ++ show i ++ " " ++ show idx ++ "/" ++ show tot))
              >> (runParser parseRaw <$> fetchRaw s i)
        )
        (zip is [(1 :: Int) ..])
        <* liftIO (endProgress cli)

class (Item a, ZhData i) => ItemContainer a i where
  type ICOpt a i
  type ICSigner a i
  fetchItemsRaw :: (MonadHttp m, MonadThrow m) => Cli -> ICOpt a i -> ICSigner a i -> a -> m [RawData i]
  parseRawChild :: a -> RawData i -> Parser i
  parseRawChild _ = parseRaw
  saveItems :: FilePath -> ICOpt a i -> a -> [i] -> IO ()
  default saveItems :: (ShowId a, ShowId i, ICOpt a i ~~ ()) => FilePath -> ICOpt a i -> a -> [i] -> IO ()
  saveItems p () s =
    traverse_ (saveData (p </> showId s </> showType @i Proxy))

fetchChildItems ::
  (ItemContainer a i, MonadHttp m, MonadThrow m, ShowId a) =>
  Cli ->
  ICOpt a i ->
  ICSigner a i ->
  a ->
  m [i]
fetchChildItems cli opt sig v =
  fmap (runParser (parseRawChild v))
    <$> fetchItemsRaw (pushHeader (showId v) cli) opt sig v