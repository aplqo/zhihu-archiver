{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module ZhArchiver.Item
  ( RawData (..),
    encodeFilePretty,
    withDirectory,
    ZhData (..),
    runParser,
    Item (..),
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
import Network.HTTP.Req
import System.Directory
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

runParser :: (ZhData a) => RawData a -> a
runParser v =
  case parse parseRaw v of
    Success r -> r
    Error e -> error ("Parse error:" ++ e)

class (ZhData a) => Item a where
  type IId a
  type Signer a
  fetchRaw :: (MonadHttp m, MonadThrow m) => Signer a -> IId a -> m (RawData a)

fetchItems :: (Item a, Show (IId a), MonadHttp m, MonadThrow m) => Cli -> Signer a -> [IId a] -> m [a]
fetchItems cli s is =
  let tot = length is
   in traverse
        ( \(i, idx) ->
            liftIO (showProgress cli ("Fetching " ++ show i ++ " " ++ show idx ++ "/" ++ show tot))
              >> (runParser <$> fetchRaw s i)
        )
        (zip is [(1 :: Int) ..])
        <* liftIO (endProgress cli)

class (Item a, ZhData i) => ItemContainer a i where
  type ICOpt a i
  type ICSigner a i
  fetchItemsRaw :: (MonadHttp m, MonadThrow m) => Cli -> ICOpt a i -> ICSigner a i -> a -> m [RawData i]
  saveItems :: FilePath -> ICOpt a i -> a -> [i] -> IO ()

fetchChildItems ::
  (ItemContainer a i, MonadHttp m, MonadThrow m, ShowId a) =>
  Cli ->
  ICOpt a i ->
  ICSigner a i ->
  a ->
  m [i]
fetchChildItems cli opt sig v =
  fmap runParser
    <$> fetchItemsRaw (pushHeader (showValId v) cli) opt sig v